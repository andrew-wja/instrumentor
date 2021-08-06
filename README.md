# instrumentor

A tool for instrumenting LLVM IR and bitcode

## Getting set up

`instrumentor` uses a local build of LLVM for ease of deployment.

A `Makefile` is provided to build `instrumentor` and dependencies. Just say
`make` in the top level of the repository.

## Using `instrumentor`

If you followed the build instructions above, you will have built the
`instrumentor` executable at `dist/instrumentor` and the runtimes at
`dist/runtimes/release`.

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
# compile
%.bc: %.c
    clang -emit-llvm -c -o $@ $< $(CFLAGS)

# instrument
%.instrumented.bc: %.bc
    cp $< $@
    instrumentor <options> $@

# link
%.o: %.instrumented.bc
    clang -fuse-ld=lld -flto <runtime>.a $< -o $@
```

If you do not wish to install the runtimes and headers shipped with
`instrumentor` on your system's include path, you can use environment variables
such as `LD_LIBRARY_PATH` to temporarily extend search paths at build time.
Examples of how to do this can be found in the `test` directory, which uses
separate compilation to build the test programs and their instrumented
versions. If you have not installed the runtime shared libraries system-wide,
you need to ensure that the dynamic linker can find them in
`dist/runtimes/{release,debug}` when your executable is launched.

### Runtime Selection

The `instrumentor` runtimes are simple shared libraries which implement the
book-keeping required to check various properties of the program at runtime.
Since the tool generates calls to these runtime functions when instrumenting
your program, you will need to link your final executable with one of the
runtime libraries in order to actually execute the instrumented version of your
code.

`instrumentor` provides different versions of each runtime depending on what
you would like to accomplish. If you want full checking of your program, you
will need to link the `_full.a` version of the runtime you wish to use.

If you want to benchmark the performance of `instrumentor`, you can link
against the `_bench.a` version of the runtime. This version does everything
that the full version of the runtime does except that the checks which detect
errors always succeed, so that no errors are reported. For benchmarking
purposes, this is useful if you are trying to compare `instrumentor` to another
tool that allows a program to continue execution even after errors are
detected.

If you would like to use a dry-run approach as a placeholder while you are
working on integrating `instrumentor` with your build, you can use the
`_nochecks.a` version of the runtime, which as the name suggests, does not
actually perform any checks, and as such, will not change the behaviour of your
program at execution time.

## Modifying and Extending `instrumentor`

### Building `instrumentor` manually

Since we are using a local build of LLVM, you will either need to modify
`$PATH` and `$LD_LIBRARY_PATH` permanently or prefix your shell commands, for
example:

```
LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$PATH stack build
```
