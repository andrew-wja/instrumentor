.PHONY: all clean install

all: libSoftBoundCETS_full.a libSoftBoundCETS_bench.a

CFLAGS+=-Wall -pedantic -O3 -march=native -fno-strict-aliasing

ARFLAGS=-rcs

clean:
	rm -f *.bc *.a *.o

%.bc: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -flto -c $< -o $@
%.o: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -c $< -o $@

libSoftBoundCETS_full.a: softboundcets-common.bc softboundcets-init.bc softboundcets-internal.bc softboundcets-wrappers.bc softboundcets-interface.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^
# If lld is not available and the system version of the gold linker supports LTO:
#libSoftBoundCETS_full.a: softboundcets-common.o softboundcets-init.o softboundcets-internal.o softboundcets-wrappers.o softboundcets-interface.o
#	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH ar --plugin=$(realpath ../../llvm-root/lib/LLVMgold.so) ${ARFLAGS} $@ $^
libSoftBoundCETS_full.so: softboundcets-common.o softboundcets-init.o softboundcets-internal.o softboundcets-wrappers.o softboundcets-interface.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^

%.bench.bc: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -DSOFTBOUNDCETS_BENCHMARKING_MODE -flto -c $< -o $@
%.bench.o: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -DSOFTBOUNDCETS_BENCHMARKING_MODE -c $< -o $@

libSoftBoundCETS_bench.a: softboundcets-common.bench.bc softboundcets-init.bench.bc softboundcets-internal.bench.bc softboundcets-wrappers.bench.bc softboundcets-interface.bench.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^
# If lld is not available and the system version of the gold linker supports LTO:
#libSoftBoundCETS_bench.a: softboundcets-common.bench.o softboundcets-init.bench.o softboundcets-internal.bench.o softboundcets-wrappers.bench.o softboundcets-bench.bench.o
#	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH ar --plugin=$(realpath ../../llvm-root/lib/LLVMgold.so) ${ARFLAGS} $@ $^
libSoftBoundCETS_bench.so: softboundcets-common.bench.o softboundcets-init.bench.o softboundcets-internal.bench.o softboundcets-wrappers.bench.o softboundcets-interface.bench.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^

%.nochecks.bc: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -DSOFTBOUNDCETS_NOCHECKS_MODE -flto -c $< -o $@
%.nochecks.o: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -DSOFTBOUNDCETS_NOCHECKS_MODE -c $< -o $@

libSoftBoundCETS_nochecks.a: softboundcets-common.nochecks.bc softboundcets-init.nochecks.bc softboundcets-internal.nochecks.bc softboundcets-wrappers.nochecks.bc softboundcets-interface.nochecks.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^
# If lld is not available and the system version of the gold linker supports LTO:
#libSoftBoundCETS_nochecks.a: softboundcets-common.nochecks.o softboundcets-init.nochecks.o softboundcets-internal.nochecks.o softboundcets-wrappers.nochecks.o softboundcets-nochecks.nochecks.o
#	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH ar --plugin=$(realpath ../../llvm-root/lib/LLVMgold.so) ${ARFLAGS} $@ $^
libSoftBoundCETS_nochecks.so: softboundcets-common.nochecks.o softboundcets-init.nochecks.o softboundcets-internal.nochecks.o softboundcets-wrappers.nochecks.o softboundcets-interface.nochecks.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^

install: libSoftBoundCETS_full.a libSoftBoundCETS_full.so libSoftBoundCETS_bench.a libSoftBoundCETS_bench.so libSoftBoundCETS_nochecks.a libSoftBoundCETS_nochecks.so
	mkdir -p ${DESTDIR}/lib
	cp $^ ${DESTDIR}/lib
	cp blacklist ${DESTDIR}/blacklist.SoftboundCETS
