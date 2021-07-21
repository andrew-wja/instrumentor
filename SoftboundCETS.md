# SoftboundCETS

`SoftboundCETS` is a combination of two pieces of research done by [Santosh
Nagarakatte et al. at
Rutgers](https://www.cs.rutgers.edu/~santosh.nagarakatte/softbound/).

`Softbound` is a spatial safety mechanism, and `CETS` (Compiler-Enforced
Temporal Safety) is a temporal safety mechanism. `CETS` relies on preexisting
assurance of spatial safety to provide temporal safety guarantees. Together,
they can provide complete memory safety for single-threaded code.

## How SoftboundCETS Works In Theory

`SoftboundCETS` associates metadata with pointers and checks this metadata at
runtime. There are four pieces of metadata associated with any instrumented
pointer. The `base` and `bound` addresses are memory locations such that `base
<= ptr < bound` always holds. Note that the interval is specifically inclusive
of the base address and exclusive of the bound address.

The `key` is an integer value which is unique to each allocated object (on both
the stack and the heap), and the `lock` is a pointer to a disjoint metadata
space, where the key value resides. When a pointer is dereferenced, the `lock`
and `key` value associated with the pointer are used to check that the key
value in the disjoint metadata space still matches the key associated with the
pointer. Since deallocating an object sets the key in disjoint metadata space
to `0`, and `0` is a reserved key value, uses of pointers after free can be
detected.

Additionally, each stack frame is assigned a unique integer `key` and a `lock`
from the same key pool as the allocation keys. When a function creates
local variables on the stack, they all share the function's lock and key. When
the function returns, the key in disjoint metadata space is again set to `0`.
If the address of a stack allocated local variable escapes, a key mismatch
will be detected whenever it is used.

Global variables are assigned the reserved key value `1` and a global `lock` is
allocated for them in the runtime. It is an error to attempt to deallocate any
entity with the key value `1`; these entities are allocated by the linker.

When pointers are passed to or returned from functions, their associated
metadata is placed on a "shadow stack", residing in another disjoint metadata
space. Inside a function, metadata for pointer arguments is available by
loading it from the shadow stack. Metadata for a returned pointer is passed
back to the caller on the shadow stack.

## How SoftboundCETS Works In Practice

There are three disjoint metadata spaces: the metadata space for pointer
function arguments (aka "shadow stack"), the metadata space for stack frame
keys, and the general metadata space for pointers stored to memory.

However, there is also a fourth "space" where metadata lives: in local
variables while it is being used to perform checks. While the three disjoint
metadata spaces are managed by the runtime, the fourth "space" is managed
entirely by the compiler at instrumentation time. This aspect is not discussed
in much detail in the original papers. In particular, to get good performance,
it is desirable to prevent metadata from being read from and written to memory
as much as possible. This implies aggressive caching of metadata values in
local variables, which the compiler can then promote to registers at
instruction selection time.

It is helpful to categorize LLVM instructions into two classes: those which can
cause metadata to be allocated in local variables, and those which cannot.
There are very few situations which can require local metadata variables to be
allocated.

1. Loading a pointer from memory requires local metadata variables to
be allocated for the `base`, `bound`, `key`, and `lock` for that pointer, which
are retrieved from the runtime.

2. Calling a function which returns a pointer
requires local metadata variables to be allocated to hold the metadata for that
pointer, which is popped from the shadow stack.

3. Calling a function which takes
pointer arguments requires the metadata for those pointer arguments to be in
local variables at the call site, so that the metadata can be pushed onto the
shadow stack.

4. Allocation of data on the stack with the `alloca`
instruction causes local metadata variables for that allocation (which in LLVM
is explictly held by reference to its address) to be created.

5. Incoming values to a `phi` or `select` instruction must have their metadata
available in local variables in order to select the correct metadata at
runtime.

6. Returning a pointer from a function requires the metadata for that pointer
to be allocated in local variables in order for it to be pushed to the shadow
stack.

All other instructions either have no effect on the metadata or merely
introduce aliases, where multiple pointers share the same allocated metadata.
For example, if a pointer is `bitcast` to a different type, both pointers share
the same local metadata variables.

## Qualitative Implementation Concerns

A key implementation issue for the SoftboundCETS algorithm is the method for
handling _aggregate_ datatypes, such as structures, arrays, and SIMD vectors.
All of these datatypes may contain additional pointers which need to be
instrumented.

When encountering aggregate datatypes, a naive approach is to treat them simply
as data. This is sufficient to ensure that accesses via a pointer to an
individual instance of a datatype are safe. It is also
sufficient to ensure that accesses via any of the contained pointers are safe,
because these contained pointers will eventually become local variables
where they are used, which automatically exposes them to instrumentation.

However, the naive approach does not allow an important case to be handled:
aliasing a pointer to a field of an aggregate. Consider the following short
example.

```
typedef struct {
  int a;
  float b;
} foo;

void bad(foo *f) {
  int *x = &(f->a);
  float y = *(x+1);
}
```

Although `sizeof(int)` and `sizeof(float)` is very likely the same value, this
is technically incorrect code. It is illegal in C to dereference the `int`
pointer `x` as a `float` pointer, since these effective types are not
convertible. Thus the increment to `x` which moves it to point to the next
field produces a pointer which is not valid. However, if we treat the struct
`foo` as simple data, the access `*(x+1)` looks valid -- it is both temporally
and spatially in bounds of the structure. However, it is not spatially in
bounds of the _field_ `a`, which is the core of the issue.

In order to deal with this case, it is necessary to perform *bounds narrowing*
checks whenever a pointer is derived from another pointer whose referent is a
field of an aggregate data type. In this case, the bound on `int *x` should be
narrowed in the statement which creates the pointer to `(char*)x + sizeof(int)`
rather than simply inherited from the parent pointer `f`, whose bound is
`(char*)x + sizeof(foo)`.

Bounds narrowing introduces extra memory overhead, because now pointer `x`
requires disjoint metadata from the parent pointer `f`, which must be allocated
in local variables. However, no extra overhead in the runtime disjoint metadata
space is incurred because metadata is written to that space unconditionally
whenever a pointer is stored to memory.


## Quantitative Implementation Concerns (Performance and Overhead)

### Dominated Spatial Check Elimination

When one load or store is dominated by another, all execution paths leading to
the second operation include the first operation. In this case, the spatial
checks for the second (and subsequent) dominated operation(s) with the same
target address can be eliminated. The spatial check generated for the first
operation will either have succeeded or caused execution to abort, so the
second and subsequent dominated operations are either correct or will not be
executed. This optimization is implemented in the original SoftboundCETS.

### Dominated Temporal Check Elimination

When multiple load or store instructions access the same _root object_, only
the first needs to perform temporal checks, because the result of the check
will be the same for the others. This optimization is only correct if we can
determine that there is no intervening call to `free()` the root object.

This optimization is present in the original SoftboundCETS implementation, but
it does not include an escape analysis to check for calls to `free()` and is
therefore *not correct*. Only the first access to any given root object checks
if the object is allocated, and all subsequent accesses to the same root object
are not instrumented with temporal checks. If the root object being accessed is
freed after the first access but before the other accesses, the subsequent
accesses will not be recognized as use-after-free.

The original SoftboundCETS implementation performs this optimization at the
basic block level and at the function level. With the addition of an escape
analysis, this optimization would be correct.

### Derived Pointer Metadata Storage Optimization

When one pointer is derived from another without bounds narrowing both have the
same metadata. When both pointers also have matching temporal and spatial
extents (for example, both point to the same array), a storage optimization is
possible.

When the first pointer is written to memory and we have stored the metadata for
that pointer to the runtime disjoint metadata space, all subsequent pointers
written to memory with the same metadata and temporal and spatial extents could
simply use a *pointer* to the metadata record of the first. This could
potentially save a lot of space in pointer-heavy code, because metadata is 4x
larger than a pointer. This optimization is not implemented in the original
SoftboundCETS.

### Effective Type Cast Optimization

When pointers are cast to different types, the effective typing rules of C come
into play. The safe thing to do in every case is to treat casts of pointers as
unsafe. If the parent pointer was a safe pointer, casting that pointer to any
other type creates a pointer which now requires checking. For example, consider
the following two snippets of C code.

```
char *t = malloc(1);
*t = 'a';
```

```
int *t = malloc(sizeof(int));
*t = 1;
```

Because the type of pointer returned by `malloc` is `void*`, which is
convertible to `char*`, the first snippet does not imply a cast. However, the
second snippet does imply a cast, from `void*` to `int*`. Most C programmers
would think of these two pieces of code as being the same, but in fact they are
not!

Whenever we cast a pointer from one type to another, it is possible for us to
cause problems. For example, if we cast a `char*` to an `int*`, we will be able
to access a number of extra bytes of memory through the `int*`.

In order to prevent this, we unconditionally insert checks for pointers
resulting from casts. Specifically with reference to the above snippets, in
the first snippet the pointer returned by `malloc` is in the `SAFE` pointer
class, and since converting it to a `char*` does not imply an actual cast, it
remains in the `SAFE` pointer class. There are no intervening uses of the pointer
which could cause deallocation to occur, and thus the dereference `*t = 'a'` is
a dereference of a `SAFE` pointer and does not cause the generation of checks.

However, the pointer in the second snippet is converted to `int*`, which does
involve a cast. The cast results in an `UNSAFE` pointer, and so the dereference
`*t = 1` is of an `UNSAFE` pointer, which causes the generation of checks.

In order to introduce the minimal quantity of checks, we should consult the C
effective typing rules to determine when a cast preserves the membership of the
derived pointer in the `SAFE` class. For example, in the second snippet above,
the spatial bound of the allocation is not changed by the cast, since
`sizeof(int)` bytes were allocated. Since the bounds are identical, this cast
is spatially safe. This optimization is not implemented in the original
SoftboundCETS.

### Escape To Deallocation Optimization

Whenever a pointer escapes the local scope (i.e. the basic block) where it was
created, it is possible that it could be used to deallocate the memory that it
points to (if that memory was dynamically allocated). For example, consider the
following code snippet.

```
char *t = malloc(1);
*t = 'a';
printf("%p is %c\n", t, *t);
*t = 'b';
```

Here, the pointer `t` *escapes* the basic block by being passed as an argument
to the function `printf`. Without doing some analysis, we cannot be certain
that `printf` will not cause the deallocation of the memory pointed to by `t`.
Thus, we must insert checks for the dereference `*t = 'b';` because we can no
longer see that it is evidently safe. Whenever a pointer escapes the basic
block, it is moved from the `SAFE` to the `UNSAFE` pointer class.

With some analysis, it is possible to determine that `printf()` in fact does
*not* call `free()` on `t`, and thus `t`'s membership in the `SAFE` class is
preserved across the escape to `printf()`. However, we currently assume
(conservatively) that all escapes are potential deallocations. This
optimization is not implemented in the original SoftboundCETS.

### Register Allocation of Metadata

Metadata for local variables is stored on the stack in more local variables.
However, that metadata also lives in registers while it is being created. In
the case where the metadata corresponds to a `SAFE` pointer, we know that it
has not changed since it was created. This means we can keep it live in
registers, rather than reload it from local variables. Consider the following
snippet. We create local variables with `alloca` for the metadata for a 16 byte
allocation `%4`. Subsequently we do various things, none of which cause `%4` to
change class. When we come to `free()` the memory pointed to by `%4`, we could
reuse the metadata which we have in registers `%sbcets_43..46`, extending their
live ranges. However, the conservative assumption is that there always exists
an escape of `%4` which could invalidate the metadata held in registers. This
means the metadata needs reloading from where it is stored in local variables.


```
...

%sbcets_14 = alloca i8*, align 8
%sbcets_15 = alloca i8*, align 8
%sbcets_16 = alloca i64, align 8
%sbcets_17 = alloca i8*, align 8
...
br label %body

...

%body:

...

%4 = tail call noalias i8* @malloc(i64 16)
%sbcets_43 = call i8* @__softboundcets_load_base_shadow_stack(i32 0)
%sbcets_44 = call i8* @__softboundcets_load_bound_shadow_stack(i32 0)
%sbcets_45 = call i64 @__softboundcets_load_key_shadow_stack(i32 0)
%sbcets_46 = call i8* @__softboundcets_load_lock_shadow_stack(i32 0)
store i8* %sbcets_43, i8** %sbcets_14, align 8
store i8* %sbcets_44, i8** %sbcets_15, align 8
store i64 %sbcets_45, i64* %sbcets_16, align 8
store i8* %sbcets_46, i8** %sbcets_17, align 8
call void @__softboundcets_deallocate_shadow_stack_space()

...

call void @__softboundcets_allocate_shadow_stack_space(i32 2)
%sbcets_63 = load i8*, i8** %sbcets_14
%sbcets_64 = load i8*, i8** %sbcets_15
%sbcets_65 = load i64, i64* %sbcets_16
%sbcets_66 = load i8*, i8** %sbcets_17
call void @__softboundcets_store_base_shadow_stack(i8* %sbcets_63, i32 1)
call void @__softboundcets_store_bound_shadow_stack(i8* %sbcets_64, i32 1)
call void @__softboundcets_store_key_shadow_stack(i64 %sbcets_65, i32 1)
call void @__softboundcets_store_lock_shadow_stack(i8* %sbcets_66, i32 1)
call void @free(i8* %4)
call void @__softboundcets_deallocate_shadow_stack_space()
```

The reloading of metadata into fresh registers `%sbcets_63..66` here could be
eliminated with some analysis, improving performance at the cost of increased
register pressure from the extension of the live ranges of the original
registers `%sbcets_43..46`.

```
call void @__softboundcets_allocate_shadow_stack_space(i32 2)
call void @__softboundcets_store_base_shadow_stack(i8* %sbcets_43, i32 1)
call void @__softboundcets_store_bound_shadow_stack(i8* %sbcets_44, i32 1)
call void @__softboundcets_store_key_shadow_stack(i64 %sbcets_45, i32 1)
call void @__softboundcets_store_lock_shadow_stack(i8* %sbcets_46, i32 1)
call void @free(i8* %4)
call void @__softboundcets_deallocate_shadow_stack_space()
```

This optimization is not implemented in the original SoftboundCETS.

### Realloc Dangling Pointer Improvement

In C, when someone uses `realloc`, pointers to the old allocation other than
the pointer passed to `realloc` become dangling pointers. If the implementation
of `realloc` extends the old allocation in-place, they will continue to work,
but if the implementation of `realloc` moves the allocation, they will point to
invalid memory. In this case it is as if they point to a freed allocation.

If we treat realloc as if it _always_ moves the allocation, we can catch these
dangling pointers. We would do this by giving the pointer returned by `realloc`
a fresh key instead of reusing the key from the old allocation.

This is not done in the original SoftboundCETS implementation.
