.PHONY: all ensure-submodules build-llvm build-debug-runtimes build-release-runtimes build-instrumentor dist/instrumentor clean

all: ensure-submodules build-llvm build-release-runtimes build-instrumentor

ensure-submodules:
	git submodule update --init --recursive

build-llvm:
	./scripts/build-llvm.sh

build-debug-runtimes:
	./scripts/build-runtimes.sh Debug

build-release-runtimes:
	./scripts/build-runtimes.sh Release

build-instrumentor:
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack build

dist/instrumentor: build-instrumentor
	mkdir -p dist
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack --local-bin-path dist install

clean:
	rm -rf dist
	stack clean

really-clean: clean
	rm -rf llvm-build runtimes-build
