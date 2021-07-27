.PHONY: all tools clean

all: tools test instrumented dummy-instrumented

tools:
	$(MAKE) -C ../../ dist/instrumentor dist/runtimes/debug

clean:
	rm -f test.{ll,bc} test instrumented.{ll,bc,s,dump,opt.s} instrumented dummy-instrumented


# Simple uninstrumented version of the test program
test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -c test.c -emit-llvm -S -o test.ll

test.bc: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -flto -c test.c -emit-llvm -o test.bc

test.s: test.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 test.ll -S -o test.s

test.dump: test
	objdump -D test > test.dump

test: test.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 test.bc -o test

run-test: test
	./test

debug-test: test
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb test


# Instrumented version of the test program
instrumented.ll: test.ll
	cp test.ll instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/debug/blacklist.SoftboundCETS) --checks --register-metadata --stack --load --store --call --bitcast instrumented.ll

instrumented.bc: test.bc
	cp test.bc instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/debug/blacklist.SoftboundCETS) --checks --register-metadata --stack --load --store --call --bitcast instrumented.bc

instrumented.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 instrumented.ll -S -o instrumented.s

instrumented.opt.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 instrumented.ll -S -o instrumented.opt.s

instrumented.dump: instrumented
	objdump -D instrumented > instrumented.dump

instrumented.lto.dump: instrumented-lto
	objdump -D instrumented-lto > instrumented.lto.dump

instrumented-lto: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -march=native -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/debug) -l:libSoftBoundCETS_full_rt.bc instrumented.bc -o instrumented-lto

instrumented: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -march=native -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/debug) -l:libSoftBoundCETS_full_rt.so instrumented.bc -o instrumented

run-instrumented: instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) ./instrumented

run-lto-instrumented: instrumented-lto
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) ./instrumented-lto

debug-instrumented: instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb instrumented


# Instrumented version of the test program linked with a dummy runtime that does no checking (for debugging purposes)
dummy-instrumented: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -march=native -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/debug) -l:libSoftBoundCETS_dummy_rt.bc instrumented.bc -o dummy-instrumented

run-dummy-instrumented: dummy-instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) ./dummy-instrumented

debug-dummy-instrumented: dummy-instrumented
	LD_LIBRARY_PATH=$(realpath ../../runtimes-build/lib) gdb dummy-instrumented
