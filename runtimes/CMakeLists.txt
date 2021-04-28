cmake_minimum_required(VERSION 3.19)
project(SoftBoundCETS)

# Set this to a valid LLVM installation dir
set(LLVM_INSTALL_DIR "" CACHE PATH "LLVM installation directory")

# A bit of a sanity checking
set(LLVM_INCLUDE_DIR "${LLVM_INSTALL_DIR}/include/llvm")
if(NOT EXISTS "${LLVM_INCLUDE_DIR}")
message(FATAL_ERROR
  " LLVM_INSTALL_DIR (${LLVM_INCLUDE_DIR}) is invalid.")
endif()

set(LLVM_CMAKE_FILE "${LLVM_INSTALL_DIR}/lib/cmake/llvm/LLVMConfig.cmake")
if(NOT EXISTS "${LLVM_CMAKE_FILE}")
message(FATAL_ERROR
    " LLVM_CMAKE_FILE (${LLVM_CMAKE_FILE}) is invalid.")
endif()

#===============================================================================
# 2. LOAD LLVM CONFIGURATION
#    For more: http://llvm.org/docs/CMake.html#embedding-llvm-in-your-project
#===============================================================================
# Add the location of LLVMConfig.cmake to CMake search paths (so that
# find_package can locate it)
list(APPEND CMAKE_PREFIX_PATH "${LLVM_INSTALL_DIR}/lib/cmake/llvm/")

find_package(LLVM REQUIRED CONFIG)
if(NOT "9" VERSION_LESS_EQUAL "${LLVM_VERSION_MAJOR}")
  message(FATAL_ERROR "Found LLVM ${LLVM_VERSION_MAJOR}, but need LLVM 9")
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_INSTALL_DIR}")

message("LLVM STATUS:
  Definitions ${LLVM_DEFINITIONS}
  Includes    ${LLVM_INCLUDE_DIRS}
  Libraries   ${LLVM_LIBRARY_DIRS}
  Targets     ${LLVM_TARGETS_TO_BUILD}"
)

# Set the LLVM header and library paths
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
add_definitions(${LLVM_DEFINITIONS})

#===============================================================================
# 3. OUR BUILD CONFIGURATION
#===============================================================================
# Use the same C++ standard as LLVM does
set(CMAKE_CXX_STANDARD 14 CACHE STRING "")
set(CMAKE_C_STANDARD 11 CACHE STRING "")

# Build type
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE
      STRING "Build type (default Debug):" FORCE)
endif()

# Compiler flags
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS} -Wall -Werror -pedantic-errors\
    -fdiagnostics-color=always -DSOFTBOUNDCETS_DEBUG -DSOFTBOUNDCETS_ASSERTS")

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS} -Wall -pedantic-errors\
    -fdiagnostics-color=always -DSOFTBOUNDCETS_DEBUG -DSOFTBOUNDCETS_ASSERTS")

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS} -Wall -Werror -pedantic-errors\
    -fdiagnostics-color=always")

set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS} -Wall -pedantic-errors\
    -fdiagnostics-color=always")

# LLVM is normally built without RTTI. Be consistent with that.
if(NOT LLVM_ENABLE_RTTI)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
endif()

# -fvisibility-inlines-hidden is set when building LLVM and on Darwin warnings
# are triggered without this flag (though otherwise it
# builds fine). For consistency, add it here too.
include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-fvisibility-inlines-hidden" SUPPORTS_FVISIBILITY_INLINES_HIDDEN_FLAG)
if (${SUPPORTS_FVISIBILITY_INLINES_HIDDEN_FLAG} EQUAL "1")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fvisibility-inlines-hidden")
endif()

# Set the build directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")

#===============================================================================
# 4. ADD SUB-TARGETS
#===============================================================================
add_subdirectory(SoftboundCETS)
