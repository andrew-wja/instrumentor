# instrumentor

A tool for instrumenting LLVM IR and bitcode

## Getting set up

`instrumentor` uses a local build of LLVM 12 (specifically the `release/12.x`
branch), because most distributions have not yet started shipping LLVM 12.

A `Makefile` is provided to build `instrumentor` and dependencies. Just say
`make` in the top level of the repository.

## Using instrumentor

Since we are using a local build of LLVM, either modify `$PATH` and
`$LD_LIBRARY_PATH` permanently or prefix your shell commands, for example:

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack build
```

To instrument a given LLVM bitcode module, say

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack exec -- instrumentor target.bc
```

To play with a simple example, `make test.ll instrumented.ll` in the `test`
subdirectory and compare the output files.
