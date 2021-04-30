add_library(SoftBoundCETS_full_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-interface.c softboundcets-wrappers.c softboundcets-init.c)
add_library(SoftBoundCETS_minimal_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-interface.c softboundcets-init.c)
add_library(SoftBoundCETS_dummy_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-dummy.c softboundcets-wrappers.c softboundcets-init.c)
add_library(SoftBoundCETS_minimal_dummy_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-dummy.c softboundcets-init.c)
