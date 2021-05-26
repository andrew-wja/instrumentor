.PHONY: all ensure-submodules patch-llvm build-llvm build-debug-runtimes build-release-runtimes build-instrumentor dist/instrumentor dist/runtimes/release dist/runtimes/debug test debug-test dist-clean test-clean clean really-clean

all: ensure-submodules build-llvm build-release-runtimes build-instrumentor dist/instrumentor dist/runtimes/release

ensure-submodules:
	git submodule update --init --recursive

patch-llvm:
	mkdir -p llvm-patches
	cd llvm-patches && wget https://708430.bugs.gentoo.org/attachment.cgi?id=662686 -O- > glibc-2.31.patch
	patch -p0 -d llvm-project <llvm-patches/glibc-2.31.patch

build-llvm: patch-llvm
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

dist/runtimes/release: build-release-runtimes
	mkdir -p $@
	cp -r runtimes-build/lib/* dist/runtimes/release/
	for x in `ls runtimes --hide='*.txt'`; do cp runtimes/$$x/blacklist dist/runtimes/release/blacklist.$$x; done

dist/runtimes/debug: build-debug-runtimes
	mkdir -p $@
	cp -r runtimes-build/lib/* dist/runtimes/debug/
	for x in `ls runtimes --hide='*.txt'`; do cp runtimes/$$x/blacklist dist/runtimes/debug/blacklist.$$x; done

test: build-release-runtimes
	@for x in `ls test`; do echo; printf "\x1b[32;1mRunning test case $$x\x1b[0m\n\n"; $(MAKE) -C test/$$x run-instrumented; done

debug-test: build-debug-runtimes
	for x in `ls test`; do $(MAKE) -C test/$$x run-instrumented; done

dist/doc:
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack haddock --haddock-arguments '--hyperlinked-source --odir=dist/doc'

dist-clean:
	rm -rf dist*

test-clean:
	for x in `ls test`; do $(MAKE) -C test/$$x clean; done

clean: dist-clean test-clean
	stack clean

really-clean: clean
	rm -rf llvm-patches llvm-build runtimes-build
	cd llvm-project && git reset --hard HEAD
