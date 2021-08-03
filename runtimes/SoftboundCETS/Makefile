.PHONY: all clean install

all: libSoftBoundCETS_full.a libSoftBoundCETS_dummy.a

CFLAGS+=-Wall -pedantic -O3 -march=native -fno-strict-aliasing

ARFLAGS=-rcs

clean:
	rm -f *.bc

%.o: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang ${CFLAGS} -flto -c $< -o $@

libSoftBoundCETS_full.a: softboundcets-common.o softboundcets-init.o softboundcets-internal.o softboundcets-wrappers.o softboundcets-interface.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^

libSoftBoundCETS_dummy.a: softboundcets-common.o softboundcets-init.o softboundcets-internal.o softboundcets-wrappers.o softboundcets-dummy.o
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-ar ${ARFLAGS} $@ $^

install: libSoftBoundCETS_full.a libSoftBoundCETS_dummy.a
	mkdir -p ${DESTDIR}/lib
	cp $^ ${DESTDIR}/lib
	cp blacklist ${DESTDIR}/blacklist.SoftboundCETS
