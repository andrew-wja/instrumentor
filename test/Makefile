test.bc: test.c
	clang -c test.c -emit-llvm -o test.bc

test: test.bc
	stack exec -- instrumentor test.bc
	llc test.bc -o test
