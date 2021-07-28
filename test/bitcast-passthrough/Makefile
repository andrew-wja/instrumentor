.PHONY: all tools clean

all: tools test instrumented dummy-instrumented

tools:
	$(MAKE) -C ../../ dist/instrumentor dist/runtimes/debug

clean:
	rm -f *.exe *.bc *.ll *.s *.dump


# Simple uninstrumented version of the test program
test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 -c test.c -emit-llvm -S -o test.ll

test.s: test.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 test.ll -S -o test.s

test.dump: test
	objdump -D test > test.dump

test: test.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 test.ll -o test

run-test: test
	./test

debug-test: test
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb test


# Instrumented version of the test program
instrumented.ll: test.ll
	cp test.ll instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/debug/blacklist.SoftboundCETS) --checks --register-metadata --stack --load --store --call --bitcast instrumented.ll

instrumented.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 instrumented.ll -S -o instrumented.s

instrumented.opt.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 instrumented.ll -S -o instrumented.opt.s

instrumented-release.dump: instrumented-release
	objdump -D instrumented-release > instrumented-release.dump

instrumented-release.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 -L$(realpath ../../dist/runtimes/release) -lSoftBoundCETS_full_rt instrumented.ll -o instrumented-release.exe

instrumented-debug.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 -L$(realpath ../../dist/runtimes/debug) -lSoftBoundCETS_full_rt instrumented.ll -o instrumented-debug.exe

run-instrumented-release: instrumented-release.exe
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) ./instrumented-release.exe

debug-instrumented: instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/debug) gdb instrumented-debug.exe


# Instrumented version of the test program linked with a dummy runtime that does no checking (for debugging purposes)
dummy-instrumented-debug.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 -L$(realpath ../../dist/runtimes/debug) -lSoftBoundCETS_dummy_rt instrumented.ll -o dummy-instrumented-debug.exe

run-dummy-instrumented-debug: dummy-instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/debug) ./dummy-instrumented-debug.exe

dummy-instrumented-release.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O1 -L$(realpath ../../dist/runtimes/release) -lSoftBoundCETS_dummy_rt instrumented.ll -o dummy-instrumented-release.exe

run-dummy-instrumented-release: dummy-instrumented-release.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) ./dummy-instrumented-release.exe

debug-dummy-instrumented: dummy-instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/debug) gdb dummy-instrumented-debug.exe
