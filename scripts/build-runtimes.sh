#!/usr/bin/env bash

mkdir -p runtimes-build

pushd runtimes-build

rm -rf *

mkdir -p lib

DESTDIR=$(realpath ./lib) make -B -C ../runtimes clean install

popd
