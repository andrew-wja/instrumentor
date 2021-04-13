# instrumentor

Build LLVM 12 locally first using the `llvm-setup.sh` script. You will need to
fetch the LLVM sources using

```
git submodule update --init --recursive
```

Since `stack` will use whatever is on the system path, either modify `$PATH` and
`$LD_LIBRARY_PATH` permanently or prefix `stack` commands, for example:

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack build
```

To instrument an LLVM bitcode module, say

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack exec -- instrumentor target.bc
```

To play with a simple example, `make test.ll test.s test` in the `test` subdirectory.
