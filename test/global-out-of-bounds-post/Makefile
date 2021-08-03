.PHONY: all tools clean

all: tools test instrumented dummy-instrumented

tools:
	$(MAKE) -C ../../ dist/instrumentor dist/runtimes/release

clean:
	rm -f *.bc *.ll *.s *.dump *.exe


# Simple uninstrumented version of the test program
test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -c test.c -emit-llvm -S -o test.ll

test.bc: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -c test.c -emit-llvm -o test.bc

test.s: test.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 test.ll -S -o test.s

test.dump: test
	objdump -D test > test.dump

test: test.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 test.bc -o test

run-test: test
	./test

debug-test: test
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb test


# Instrumented version of the test program
instrumented.ll: test.ll
	cp test.ll instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/release/blacklist.SoftboundCETS) --checks --register-metadata --stack --load --store --call --bitcast instrumented.ll

instrumented.bc: test.bc
	cp test.bc instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/release/blacklist.SoftboundCETS) --checks --register-metadata --stack --load --store --call --bitcast instrumented.bc

instrumented.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 instrumented.ll -S -o instrumented.s

instrumented.opt.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 instrumented.ll -S -o instrumented.opt.s

instrumented-release.dump: instrumented-release.exe
	objdump -D instrumented-release.exe > instrumented-release.dump

instrumented-release.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -fuse-ld=lld -flto -L$(realpath ../../dist/runtimes/release/lib) -lSoftBoundCETS_full instrumented.ll -o instrumented-release.exe

instrumented-debug.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -fuse-ld=lld -flto -L$(realpath ../../dist/runtimes/debug/lib) -lSoftBoundCETS_full instrumented.ll -o instrumented-debug.exe

run-instrumented-release: instrumented-release.exe
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) ./instrumented-release.exe

debug-instrumented: instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) gdb instrumented-debug.exe


# Instrumented version of the test program linked with a dummy runtime that does no checking (for debugging purposes)
dummy-instrumented-debug.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -fuse-ld=lld -flto -L$(realpath ../../dist/runtimes/debug/lib) -lSoftBoundCETS_dummy instrumented.ll -o dummy-instrumented-debug.exe

run-dummy-instrumented-debug: dummy-instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) ./dummy-instrumented-debug.exe

dummy-instrumented-release.exe: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O0 -fuse-ld=lld -flto -L$(realpath ../../dist/runtimes/release/lib) -lSoftBoundCETS_dummy instrumented.ll -o dummy-instrumented-release.exe

run-dummy-instrumented-release: dummy-instrumented-release.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) ./dummy-instrumented-release.exe

debug-dummy-instrumented: dummy-instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) gdb dummy-instrumented-debug.exe
