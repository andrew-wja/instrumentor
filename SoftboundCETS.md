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
space, where the key value resides. When a pointer is used, the `lock` and
`key` value associated with the pointer are used to check that the key value in
the disjoint metadata space still matches the key associated with the pointer.
Since deallocating an object sets the key in disjoint metadata space to `0`,
and `0` is a reserved key value, uses of pointers after free can be detected.

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

There are three disjoint metadata spaces: the metadata space for function
arguments (aka "shadow stack"), the metadata space for stack frame keys,
and the general metadata space for allocations.

However, there is also a fourth "space" where metadata lives: in local
variables while it is being used to perform checks. While the three disjoint
metadata spaces are managed by the runtime, the fourth "space" is managed
entirely by the compiler at instrumentation-time. This aspect is not discussed
in much detail in the original papers. In particular, to get good performance,
it is desirable to prevent metadata from being read from and written to memory
as much as possible. This implies aggressive caching of metadata values in
local variables, which the compiler can then promote to registers at
instruction selection time.

It is helpful to categorize LLVM instructions into two classes: those which can
cause metadata to be allocated in local variables, and those which cannot.
There are very few situations which can require local metadata variables to be
allocated. Loading a pointer from memory requires local metadata variables to
be allocated for the `base`, `bound`, `key`, and `lock` for that pointer, which
are retrieved from the runtime. Calling a function which returns a pointer
requires local metadata variables to be allocated to hold the metadata for that
pointer, which is popped from the shadow stack. Finally, allocation of data on
the stack with the `alloca` instruction causes local metadata variables for
that allocation (which in LLVM is explictly held by reference to its address)
to be created.

All other instructions either have no effect on the metadata or merely
introduce aliases, where multiple pointers share the same allocated metadata.
For example, if a pointer is `bitcast` to a different type, both pointers share
the same local metadata variables. The most complex of these are the `select`
and `phi` instructions, which cause aliasing at runtime that is opaque to the
compiler.
