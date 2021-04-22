.PHONY: all ensure-submodules build-llvm build-runtimes build-instrumentor clean

all: ensure-submodules build-llvm build-runtimes build-instrumentor

ensure-submodules:
	git submodule update --init --recursive

build-llvm:
	./scripts/build-llvm.sh

build-runtimes:
	./scripts/build-runtimes.sh

build-instrumentor:
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack build

clean:
	stack clean
