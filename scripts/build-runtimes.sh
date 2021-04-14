#!/usr/bin/env bash

mkdir -p runtimes-build

pushd runtimes-build

cmake -G "Unix Makefiles" ../runtimes -DLLVM_INSTALL_DIR=$(realpath ../llvm-root) -DCMAKE_C_COMPILER=$(realpath ../llvm-root/bin/clang) -DCMAKE_CXX_COMPILER=$(realpath ../llvm-root/bin/clang++)

cmake --build .

popd
