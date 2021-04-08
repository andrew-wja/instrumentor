# instrumentor

Build LLVM 12 locally using the `llvm-setup.sh` script.

Since stack will use whatever is on the system path, either modify `$PATH` and
`$LD_LIBRARY_PATH` permanently or prefix stack commands, for example:

```
PATH=$(realpath ./llvm-root/bin):$PATH stack build
```

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) stack exec ...
```
