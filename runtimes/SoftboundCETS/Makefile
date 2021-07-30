.PHONY: all clean install

all: libSoftBoundCETS_full_rt.bc libSoftBoundCETS_minimal_rt.bc libSoftBoundCETS_dummy_rt.bc libSoftBoundCETS_minimal_dummy_rt.bc

clean:
	rm -f *.bc

%.bc: %.c
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH clang -march=native -O1 -flto -c $< -o $@

libSoftBoundCETS_full_rt.bc: softboundcets-common.bc softboundcets-init.bc softboundcets-interface.bc softboundcets-internal.bc softboundcets-wrappers.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-link -v $^ -o $@

libSoftBoundCETS_minimal_rt.bc: softboundcets-common.bc softboundcets-init.bc softboundcets-interface.bc softboundcets-internal.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-link -v $^ -o $@

libSoftBoundCETS_dummy_rt.bc: softboundcets-common.bc softboundcets-dummy.bc softboundcets-init.bc softboundcets-internal.bc softboundcets-wrappers.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-link -v $^ -o $@

libSoftBoundCETS_minimal_dummy_rt.bc: softboundcets-common.bc softboundcets-dummy.bc softboundcets-init.bc softboundcets-internal.bc
	LD_LIBRARY_PATH=$(realpath ../../llvm-root/lib) PATH=$(realpath ../../dist):$(realpath ../../llvm-root/bin):$$PATH llvm-link -v $^ -o $@

install: libSoftBoundCETS_full_rt.bc libSoftBoundCETS_minimal_rt.bc libSoftBoundCETS_dummy_rt.bc libSoftBoundCETS_minimal_dummy_rt.bc
	cp $^ ${DESTDIR}
