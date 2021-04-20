.PHONY: clean

test.bc: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 -c test.c -emit-llvm -o test.bc

test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 -c test.c -emit-llvm -S -o test.ll

test: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 test.c -o test

instrumented.bc: test.bc
	cp test.bc instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor instrumented.bc

instrumented.ll: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH llvm-dis instrumented.bc -o instrumented.ll

instrumented.alt.ll: test.ll
	cp test.ll instrumented.alt.ll
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor instrumented.alt.ll

instrumented.s: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 instrumented.bc -S -o instrumented.s

instrumented: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 -L$(realpath ../runtimes-build/lib/) -lSoftBoundCETS_full_rt instrumented.bc -o instrumented

instrumented.dump: instrumented
	objdump -D instrumented > instrumented.dump

clean:
	rm -f test.{bc,ll} test instrumented.{bc,ll,s,dump} instrumented
