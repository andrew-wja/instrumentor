.PHONY: all tools clean

all: tools test instrumented bench-instrumented

tools:
	$(MAKE) -C ../../ dist/instrumentor dist/runtimes/release

clean:
	rm -f *.bc *.ll *.s *.dump *.exe


# Simple uninstrumented version of the test program
test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -c test.c -emit-llvm -S -o test.ll

test.bc: test.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -c test.c -emit-llvm -o test.bc

test.s: test.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 test.ll -S -o test.s

test.dump: test
	objdump -D test > test.dump

test: test.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 test.bc -o test

run-test: test
	./test

debug-test: test
	gdb test


# Instrumented version of the test program
instrumented.ll: test.ll
	cp test.ll instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/release/blacklist.SoftboundCETS) --checks --register-metadata --stack --load --store --call --bitcast instrumented.ll

instrumented.bc: test.bc
	cp test.bc instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/release/blacklist.SoftboundCETS) --checks --register-metadata --stack --load --store --call --bitcast instrumented.bc

instrumented.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 instrumented.ll -S -o instrumented.s

instrumented.opt.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 instrumented.ll -S -o instrumented.opt.s

instrumented-release.dump: instrumented-release.exe
	objdump -D instrumented-release.exe > instrumented-release.dump

nolto-instrumented-release.dump: nolto-instrumented-release.exe
	objdump -D nolto-instrumented-release.exe > nolto-instrumented-release.dump

instrumented-release.exe: instrumented.ll instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -flto $(realpath ../../dist/runtimes/release/lib/libSoftBoundCETS_full.a) instrumented.bc -o instrumented-release.exe

nolto-instrumented-release.exe: instrumented.ll instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/release/lib/) -lSoftBoundCETS_full instrumented.bc -o nolto-instrumented-release.exe

instrumented-debug.exe: instrumented.ll instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -flto $(realpath ../../dist/runtimes/debug/lib/libSoftBoundCETS_full.a) instrumented.bc -o instrumented-debug.exe

nolto-instrumented-debug.exe: instrumented.ll instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/debug/lib/) -lSoftBoundCETS_full instrumented.bc -o nolto-instrumented-debug.exe

run-instrumented-release: instrumented-release.exe
	./instrumented-release.exe

run-nolto-instrumented-release: nolto-instrumented-release.exe
	./nolto-instrumented-release.exe

run-instrumented-debug: instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) ./instrumented-debug.exe

run-nolto-instrumented-debug: nolto-instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) ./nolto-instrumented-debug.exe


# Instrumented version of the test program with no-op checks for debugging
nochecks-instrumented-release.dump: nochecks-instrumented-release.exe
	objdump -D nochecks-instrumented-release.exe > nochecks-instrumented-release.dump

nolto-nochecks-instrumented-release.dump: nolto-nochecks-instrumented-release.exe
	objdump -D nolto-nochecks-instrumented-release.exe > nolto-nochecks-instrumented-release.dump

nochecks-instrumented-release.exe: instrumented.ll instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -flto $(realpath ../../dist/runtimes/release/lib/libSoftBoundCETS_nochecks.a) instrumented.bc -o nochecks-instrumented-release.exe

nolto-nochecks-instrumented-release.exe: instrumented.ll instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/release/lib/) -lSoftBoundCETS_nochecks instrumented.bc -o nolto-nochecks-instrumented-release.exe

run-nochecks-instrumented-release: nochecks-instrumented-release.exe
	./nochecks-instrumented-release.exe

run-nolto-nochecks-instrumented-release: nolto-nochecks-instrumented-release.exe
	./nolto-nochecks-instrumented-release.exe


# Instrumented version of the test program for benchmarking
bench-instrumented.ll: test.ll
	cp test.ll bench-instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/release/blacklist.SoftboundCETS) --benchmark --checks --register-metadata --stack --load --store --call --bitcast bench-instrumented.ll

bench-instrumented.bc: test.bc
	cp test.bc bench-instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH instrumentor --blacklist $(realpath ../../dist/runtimes/release/blacklist.SoftboundCETS) --benchmark --checks --register-metadata --stack --load --store --call --bitcast bench-instrumented.bc

bench-instrumented-debug.dump: bench-instrumented-debug.exe
	objdump -D bench-instrumented-debug.exe > bench-instrumented-debug.dump

nolto-bench-instrumented-debug.dump: nolto-bench-instrumented-debug.exe
	objdump -D nolto-bench-instrumented-debug.exe > nolto-bench-instrumented-debug.dump

bench-instrumented-debug.exe: bench-instrumented.ll bench-instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -flto $(realpath ../../dist/runtimes/debug/lib/libSoftBoundCETS_bench.a) bench-instrumented.bc -o bench-instrumented-debug.exe

nolto-bench-instrumented-debug.exe: bench-instrumented.ll bench-instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/debug/lib/) -lSoftBoundCETS_bench bench-instrumented.bc -o nolto-bench-instrumented-debug.exe

run-bench-instrumented-debug: bench-instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/debug) ./bench-instrumented-debug.exe

run-nolto-bench-instrumented-debug: nolto-bench-instrumented-debug.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/debug) ./nolto-bench-instrumented-debug.exe

bench-instrumented-release.dump: bench-instrumented-release.exe
	objdump -D bench-instrumented-release.exe > bench-instrumented-release.dump

nolto-bench-instrumented-release.dump: nolto-bench-instrumented-release.exe
	objdump -D nolto-bench-instrumented-release.exe > nolto-bench-instrumented-release.dump

bench-instrumented-release.exe: bench-instrumented.ll bench-instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -flto $(realpath ../../dist/runtimes/release/lib/libSoftBoundCETS_bench.a) bench-instrumented.bc -o bench-instrumented-release.exe

nolto-bench-instrumented-release.exe: bench-instrumented.ll bench-instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../llvm-root/bin):$$PATH clang -O3 -fuse-ld=lld -L$(realpath ../../dist/runtimes/release/lib/) -lSoftBoundCETS_bench bench-instrumented.bc -o nolto-bench-instrumented-release.exe

run-bench-instrumented-release: bench-instrumented-release.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) ./bench-instrumented-release.exe

run-nolto-bench-instrumented-release: nolto-bench-instrumented-release.exe
	LD_LIBRARY_PATH=$(realpath ../../dist/runtimes/release) ./nolto-bench-instrumented-release.exe

