.PHONY: test.bc test.s test.ll test clean

test.bc: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -c test.c -emit-llvm -o test.bc

test.s: test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang test.bc -S -o test.s

test.ll: test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH llvm-dis test.bc -o test.ll

test: test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -lc -lm test.bc -o test

clean:
	rm -f test.bc test.s test.ll test
