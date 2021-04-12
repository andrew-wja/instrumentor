# instrumentor

Build LLVM 12 locally first using the `llvm-setup.sh` script.

Since `stack` will use whatever is on the system path, either modify `$PATH` and
`$LD_LIBRARY_PATH` permanently or prefix `stack` commands, for example:

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack build
```

To instrument an LLVM bitcode module, say

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack exec -- instrumentor target.bc
```

The instrumented code can be linked with the implementation of the instrumentation runtime using `llvm-link`

Build the instrumentation runtimes with `runtime-setup.sh`
