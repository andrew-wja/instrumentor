#!/usr/bin/env bash

mkdir -p llvm-build

pushd llvm-build

cmake -G "Unix Makefiles" ../llvm-project/llvm/ -DCMAKE_{C_COMPILER=clang,CXX_COMPILER=clang++,LINKER=ld.lld,RANLIB=$(which llvm-ranlib),AR=$(which llvm-ar),BUILD_TYPE=Release,INSTALL_PREFIX=$(realpath ../llvm-root)} \
-DLLVM_{TARGETS_TO_BUILD="X86",ENABLE_PROJECTS="clang;compiler-rt;openmp",INCLUDE_{TOOLS=ON,EXAMPLES=OFF,TESTS=OFF,BENCHMARKS=OFF},ENABLE_BINDINGS=OFF} \
-DBUILD_SHARED_LIBS=ON -DCOMPILER_RT_BUILD_{XRAY=OFF,LIBFUZZER=OFF,PROFILE=OFF,MEMPROF=OFF}

cmake --build . --parallel 6

cmake --build . --target install

popd
