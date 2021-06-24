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

5. Incoming values to a `phi` instruction must have their metadata available in
local variables in order for `phi` nodes to be generated to select the correct
metadata at runtime.

6. Returning a pointer from a function requires the metadata for that pointer
to be allocated in local variables in order for it to be pushed to the shadow
stack.

All other instructions either have no effect on the metadata or merely
introduce aliases, where multiple pointers share the same allocated metadata.
For example, if a pointer is `bitcast` to a different type, both pointers share
the same local metadata variables. The most complex of these are the `select`
and `phi` instructions, which cause aliasing at runtime that is potentially
opaque to the compiler.

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

### Derived Pointer Metadata Storage Optimization

When one pointer is derived from another without bounds narrowing both have the
same metadata. When both pointers also have matching temporal and spatial
extents (for example, both point to the same array), a storage optimization is
possible.

When the first pointer is written to memory and we have stored the metadata for
that pointer to the runtime disjoint metadata space, all subsequent pointers
written to memory with the same metadata and temporal and spatial extents could
simply use a *pointer* to the metadata record of the first. This would save a
lot of space, because metadata is 4x larger than a pointer.
