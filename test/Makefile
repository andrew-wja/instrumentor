.PHONY: test.bc test.s test clean

test.bc: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -c test.c -emit-llvm -o test.bc

test.s: test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang test.bc -S -o test.s

test: test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH stack exec -- instrumentor test.bc
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang test.bc -o test

clean:
	rm -f test.bc test.s test
