.PHONY: clean

test.bc: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -c test.c -emit-llvm -o test.bc

test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -c test.c -emit-llvm -S -o test.ll

test: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang test.c -o test

instrumented.bc: test.bc
	cp test.bc instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor instrumented.bc

instrumented.ll: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH llvm-dis instrumented.bc -o instrumented.ll

instrumented.s: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang instrumented.bc -S -o instrumented.s

instrumented: instrumented.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -lc -lm instrumented.bc -o instrumented

clean:
	rm -f test.{bc,ll} test instrumented.{bc,ll,s} instrumented
