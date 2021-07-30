#!/usr/bin/env bash

mkdir -p runtimes-build

pushd runtimes-build

rm -rf *

cmake -G "Unix Makefiles" ../runtimes -DCMAKE_BUILD_TYPE=$1 -DLLVM_INSTALL_DIR=$(realpath ../llvm-root) -DCMAKE_C_COMPILER=$(realpath ../llvm-root/bin/clang) -DCMAKE_CXX_COMPILER=$(realpath ../llvm-root/bin/clang++)

cmake --build .

# LTO bitcode files are build with make

make -B -C ../runtimes/SoftboundCETS all

popd
