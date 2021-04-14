add_library(SoftBoundCETS_full_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-interface.c softboundcets-wrappers.c)
add_library(SoftBoundCETS_minimal_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-interface.c)
add_library(SoftBoundCETS_dummy_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-dummy.c)
