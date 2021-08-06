.PHONY: all clean install

all: libSoftBoundCETS_full.a libSoftBoundCETS_bench.a

CFLAGS+=-Wall -pedantic -O3 -march=native -fno-strict-aliasing

ARFLAGS=-rcs

clean:
	rm -f *.bc *.a *.o

%.o: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -flto -c $< -o $@

libSoftBoundCETS_full.a: softboundcets-common.o softboundcets-init.o softboundcets-internal.o softboundcets-wrappers.o softboundcets-interface.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^
# If lld is not available and the system version of the gold linker supports LTO:
#libSoftBoundCETS_full.a: softboundcets-common.o softboundcets-init.o softboundcets-internal.o softboundcets-wrappers.o softboundcets-interface.o
#	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH ar --plugin=$(realpath ../../llvm-root/lib/LLVMgold.so) ${ARFLAGS} $@ $^

%.bench.o: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -DSOFTBOUNDCETS_BENCHMARKING_MODE -flto -c $< -o $@

libSoftBoundCETS_bench.a: softboundcets-common.bench.o softboundcets-init.bench.o softboundcets-internal.bench.o softboundcets-wrappers.bench.o softboundcets-interface.bench.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^
# If lld is not available and the system version of the gold linker supports LTO:
#libSoftBoundCETS_bench.a: softboundcets-common.bench.o softboundcets-init.bench.o softboundcets-internal.bench.o softboundcets-wrappers.bench.o softboundcets-bench.bench.o
#	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH ar --plugin=$(realpath ../../llvm-root/lib/LLVMgold.so) ${ARFLAGS} $@ $^

%.nochecks.o: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -DSOFTBOUNDCETS_NOCHECKS_MODE -flto -c $< -o $@

libSoftBoundCETS_nochecks.a: softboundcets-common.nochecks.o softboundcets-init.nochecks.o softboundcets-internal.nochecks.o softboundcets-wrappers.nochecks.o softboundcets-interface.nochecks.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^
# If lld is not available and the system version of the gold linker supports LTO:
#libSoftBoundCETS_nochecks.a: softboundcets-common.nochecks.o softboundcets-init.nochecks.o softboundcets-internal.nochecks.o softboundcets-wrappers.nochecks.o softboundcets-nochecks.nochecks.o
#	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH ar --plugin=$(realpath ../../llvm-root/lib/LLVMgold.so) ${ARFLAGS} $@ $^

install: libSoftBoundCETS_full.a libSoftBoundCETS_bench.a libSoftBoundCETS_nochecks.a
	mkdir -p ${DESTDIR}/lib
	cp $^ ${DESTDIR}/lib
	cp blacklist ${DESTDIR}/blacklist.SoftboundCETS
