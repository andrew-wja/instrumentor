.PHONY: clean install

clean:
	rm -f *.o *.bc *.so

%.o: %.c
	clang ${CFLAGS} -fPIC -c $< -o $@

%.bc: %.c
	clang ${CFLAGS} -flto -c $< -o $@

libSoftBoundCETS_full_rt.so: softboundcets-common.o softboundcets-init.o softboundcets-interface.o softboundcets-internal.o softboundcets-wrappers.o
	clang -shared -fPIC ${CFLAGS} $^ -o $@

libSoftBoundCETS_minimal_rt.so: softboundcets-common.o softboundcets-init.o softboundcets-interface.o softboundcets-internal.o
	clang -shared -fPIC ${CFLAGS} $^ -o $@

libSoftBoundCETS_dummy_rt.so: softboundcets-common.o softboundcets-dummy.o softboundcets-init.o softboundcets-internal.o softboundcets-wrappers.o
	clang -shared -fPIC ${CFLAGS} $^ -o $@

libSoftBoundCETS_minimal_dummy_rt.so: softboundcets-common.o softboundcets-dummy.o softboundcets-init.o softboundcets-internal.o
	clang -shared -fPIC ${CFLAGS} $^ -o $@

libSoftBoundCETS_full_rt.bc: softboundcets-common.bc softboundcets-init.bc softboundcets-interface.bc softboundcets-internal.bc softboundcets-wrappers.bc
	llvm-link -v $^ -o $@

libSoftBoundCETS_minimal_rt.bc: softboundcets-common.bc softboundcets-init.bc softboundcets-interface.bc softboundcets-internal.bc
	llvm-link -v $^ -o $@

libSoftBoundCETS_dummy_rt.bc: softboundcets-common.bc softboundcets-dummy.bc softboundcets-init.bc softboundcets-internal.bc softboundcets-wrappers.bc
	llvm-link -v $^ -o $@

libSoftBoundCETS_minimal_dummy_rt.bc: softboundcets-common.bc softboundcets-dummy.bc softboundcets-init.bc softboundcets-internal.bc
	llvm-link -v $^ -o $@

install: libSoftBoundCETS_full_rt.so libSoftBoundCETS_minimal_rt.so libSoftBoundCETS_dummy_rt.so libSoftBoundCETS_minimal_dummy_rt.so libSoftBoundCETS_full_rt.bc libSoftBoundCETS_minimal_rt.bc libSoftBoundCETS_dummy_rt.bc libSoftBoundCETS_minimal_dummy_rt.bc
	cp $^ ${DESTDIR}
