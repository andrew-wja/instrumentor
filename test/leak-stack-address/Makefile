.PHONY: all tools clean

all: tools test instrumented dummy-instrumented

tools:
	$(MAKE) -C ../../ dist/instrumentor build-debug-runtimes

clean:
	rm -f test.ll test instrumented.{ll,s,dump} instrumented dummy-instrumented


# Simple uninstrumented version of the test program
test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -c test.c -emit-llvm -S -o test.ll

test.s: test.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 test.ll -S -o test.s

test.dump: test
	objdump -D test > test.dump

test: test.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 test.ll -o test

run-test: test
	./test

debug-test: test
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb test


# Instrumented version of the test program
instrumented.ll: test.ll
	cp test.ll instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor instrumented.ll

instrumented.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 instrumented.ll -S -o instrumented.s

instrumented.dump: instrumented
	objdump -D instrumented > instrumented.dump

instrumented: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -L$(realpath ../../runtimes-build/lib/) -lSoftBoundCETS_full_rt instrumented.ll -o instrumented

run-instrumented: instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) ./instrumented

debug-instrumented: instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb instrumented


# Instrumented version of the test program linked with a dummy runtime that does no checking (for debugging purposes)
dummy-instrumented: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -L$(realpath ../../runtimes-build/lib/) -lSoftBoundCETS_dummy_rt instrumented.ll -o dummy-instrumented

run-dummy-instrumented: dummy-instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) ./dummy-instrumented

debug-dummy-instrumented: dummy-instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb dummy-instrumented
