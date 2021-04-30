# instrumentor

A tool for instrumenting LLVM IR and bitcode

## Getting set up

`instrumentor` uses a local build of LLVM for ease of deployment.

A `Makefile` is provided to build `instrumentor` and dependencies. Just say
`make` in the top level of the repository.

## Building `instrumentor`

Since we are using a local build of LLVM, either modify `$PATH` and
`$LD_LIBRARY_PATH` permanently or prefix your shell commands, for example:

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack build
```

## Using `instrumentor`

To instrument a given LLVM bitcode module, say

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack exec -- instrumentor target.bc
```

Alternatively, if you have build the executable for distribution, say

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH dist/instrumentor target.bc
```

To play with a simple example, `make -B run-test run-dummy-instrumented
run-instrumented` in one of the subdirectories below `test`. You may also like
to compare the output `.ll` files. See the `Makefile` in any of the test case
directories for more example usage, including disassembly of instrumented
native code.
