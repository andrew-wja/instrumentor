.PHONY: clean run-test run-instrumented

test.ll: test.c
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 -c test.c -emit-llvm -S -o test.ll

test: test.ll
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 test.ll -o test

run-test: test
	./test

instrumented.ll: test.ll
	cp test.ll instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../dist):$(realpath ../llvm-root/bin):$$PATH instrumentor instrumented.ll

instrumented.s: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 instrumented.ll -S -o instrumented.s

instrumented: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 -L$(realpath ../runtimes-build/lib/) -lSoftBoundCETS_full_rt instrumented.ll -o instrumented

dummy-instrumented: instrumented.ll
	LD_LIBRARY_PATH=$(realpath ../llvm-root/lib) PATH=$(realpath ../llvm-root/bin):$$PATH clang -O0 -L$(realpath ../runtimes-build/lib/) -lSoftBoundCETS_dummy_rt instrumented.ll -o dummy-instrumented

instrumented.dump: instrumented
	objdump -D instrumented > instrumented.dump

run-instrumented: instrumented
	LD_LIBRARY_PATH=$(realpath ../runtimes-build/lib) ./instrumented

run-dummy-instrumented: dummy-instrumented
	LD_LIBRARY_PATH=$(realpath ../runtimes-build/lib) ./dummy-instrumented

clean:
	rm -f test.{bc,ll} test instrumented.{alt.ll,bc,ll,s,dump} instrumented
