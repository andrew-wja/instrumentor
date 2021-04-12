add_library(SoftBoundCETS_full_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-interface.c softboundcets-wrappers.c)
add_library(SoftBoundCETS_minimal_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-interface.c)
add_library(SoftBoundCETS_dummy_rt SHARED softboundcets-internal.h softboundcets-internal.c softboundcets-interface.h softboundcets-dummy.c)

add_custom_command(OUTPUT softboundcets-internal.bc
                   COMMAND clang -c ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-internal.c -emit-llvm -o softboundcets-internal.bc
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-internal.c)
add_custom_command(OUTPUT softboundcets-interface.bc
                   COMMAND clang -c ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-interface.c -emit-llvm -o softboundcets-interface.bc
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-interface.c)
add_custom_command(OUTPUT softboundcets-wrappers.bc
                   COMMAND clang -c ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-wrappers.c -emit-llvm -o softboundcets-wrappers.bc
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-wrappers.c)
add_custom_command(OUTPUT softboundcets-dummy.bc
                   COMMAND clang -c ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-dummy.c -emit-llvm -o softboundcets-dummy.bc
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/softboundcets-dummy.c)

add_custom_command(COMMAND llvm-link softboundcets-internal.bc softboundcets-interface.bc softboundcets-wrappers.bc -o softboundcets-full-rt.bc
                   DEPENDS softboundcets-internal.bc softboundcets-interface.bc softboundcets-wrappers.bc
                   TARGET SoftBoundCETS_full_rt POST_BUILD)
add_custom_command(COMMAND llvm-link softboundcets-internal.bc softboundcets-interface.bc -o softboundcets-minimal-rt.bc
                   DEPENDS softboundcets-internal.bc softboundcets-interface.bc
                   TARGET SoftBoundCETS_minimal_rt POST_BUILD)
add_custom_command(COMMAND llvm-link softboundcets-internal.bc softboundcets-dummy.bc -o softboundcets-dummy-rt.bc
                   DEPENDS softboundcets-internal.bc softboundcets-dummy.bc
                   TARGET SoftBoundCETS_dummy_rt POST_BUILD)
