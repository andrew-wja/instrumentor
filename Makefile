.PHONY: all ensure-submodules build-llvm build-runtimes build-instrumentor dist/instrumentor clean

all: ensure-submodules build-llvm build-runtimes build-instrumentor

ensure-submodules:
	git submodule update --init --recursive

build-llvm:
	./scripts/build-llvm.sh

build-runtimes:
	./scripts/build-runtimes.sh

build-instrumentor:
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack build

dist/instrumentor: build-instrumentor
	mkdir -p dist
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack --local-bin-path dist install

clean:
	rm -rf dist
	stack clean
