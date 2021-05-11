# instrumentor

A tool for instrumenting LLVM IR and bitcode

## Getting set up

`instrumentor` uses a local build of LLVM for ease of deployment.

A `Makefile` is provided to build `instrumentor` and dependencies. Just say
`make` in the top level of the repository.

## Using `instrumentor`

If you followed the build instructions above, you will have built the
`instrumentor` executable at `dist/instrumentor` and the runtimes at
`runtimes-build`.

### Modifying Your Build

Modifying your build to include instrumentation with `instrumentor` is
relatively straightforward, but of course will depend on the complexity of the
build you begin with. If you are using something like the traditional separate
compilation workflow, integrating `instrumentor` into your build should be quite
straightforward.

Assuming you have a build rule for object files which looks something like
this:

```
%.o: %.c
    clang -c -o $@ $< $(CFLAGS)
```

Then to integrate `instrumentor` you would split the production of the object
files into two parts to accomodate the instrumentation step as follows:

```
%.bc: %.c
    clang -emit-llvm -c -o $@ $< $(CFLAGS)

%.instrumented.bc: %.bc
    cp $< $@
    instrumentor <options> $@

%.o: %.instrumented.bc
    clang -l<runtime>_full_rt $< -o $@
```

If you do not wish to install the runtimes and headers shipped with
`instrumentor` on your system's include path, you can use environment variables
such as `LD_LIBRARY_PATH` to temporarily extend search paths at build time.
Examples of how to do this can be found in the `test` directory, which uses
separate compilation to build the test programs and their instrumented
versions.

### Runtime Selection

`instrumentor`s runtimes are simple shared libraries which implement the
book-keeping required to check various properties of the program at runtime.
Since the tool generates calls to these runtime functions when instrumenting
your program, you will need to link your final executable with one of the
runtime libraries in order to actually execute the instrumented version of your
code.

`instrumentor` provides different versions of each runtime depending on what
you would like to accomplish. If you want full checking of your program, you
will need to link the `_full_rt.so` version of the runtime you wish to use. If
you want to benchmark the performance of `instrumentor`, you can link against
the `_dummy_rt.so` version of the runtime. This version does everything that
the full version of the runtime does except that the checks which detect errors
always succeed, so that no errors are reported. For benchmarking purposes, this
is useful if you are trying to compare `instrumentor` to another tool that
allows a program to continue execution even after errors are detected.

For embedded systems development, it is common to work with freestanding code
which does not use standard libraries. The runtimes with the `_minimal` prefix
are for this situation. These do not depend on standard library functionality,
and do not provide instrumentation-aware wrappers for standard library
functions.

## Modifying and Extending `instrumentor`

### Building `instrumentor` manually

Since we are using a local build of LLVM, either modify `$PATH` and
`$LD_LIBRARY_PATH` permanently or prefix your shell commands, for example:

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack build
```
