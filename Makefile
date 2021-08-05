.PHONY: all setup ensure-submodules patch-llvm build-llvm build-debug-runtimes build-release-runtimes build-instrumentor dist/runtimes/release dist/runtimes/debug dist/instrumentor dist/doc dist-clean test-clean clean really-clean llvm-clean runtimes-clean

all: dist/instrumentor dist/runtimes/release

setup: ensure-submodules build-llvm build-release-runtimes build-instrumentor

ensure-submodules:
	git submodule update --init --recursive

llvm-patches/glibc-2.31.patch:
	mkdir -p llvm-patches
	cd llvm-patches && wget https://708430.bugs.gentoo.org/attachment.cgi?id=662686 -O- > glibc-2.31.patch

patch-llvm: llvm-patches/glibc-2.31.patch
	if ! patch -R --dry-run -s -f -p0 -d llvm-project <llvm-patches/glibc-2.31.patch 2>/dev/null 1>&2; then patch -s -f -p0 -d llvm-project <llvm-patches/glibc-2.31.patch; fi

build-llvm: patch-llvm
	./scripts/build-llvm.sh

build-instrumentor:
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack build

dist/instrumentor: build-instrumentor
	mkdir -p dist
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack --local-bin-path dist install

dist/runtimes/release: DESTDIR = `realpath $@`
dist/runtimes/release:
	mkdir -p $@
	${MAKE} -B -C ./runtimes all
	${MAKE} DESTDIR=${DESTDIR} -C ./runtimes install

dist/runtimes/debug: DESTDIR = `realpath $@`
dist/runtimes/debug:
	mkdir -p $@
	${MAKE} -B -C ./runtimes all CFLAGS="-g -DSOFTBOUNDCETS_DEBUG"
	${MAKE} DESTDIR=${DESTDIR} -C ./runtimes install

dist/doc:
	LD_LIBRARY_PATH=$(realpath ./llvm-root/lib) PATH=$(realpath ./llvm-root/bin):$$PATH stack haddock --haddock-arguments '--hyperlinked-source --odir=dist/doc'


test: dist/runtimes/release
	@for x in `ls test`; do echo; printf "\x1b[32;1mRunning test case $$x\x1b[0m\n\n"; $(MAKE) -C test/$$x instrumented-release.dump run-instrumented-release; done

bench-test: dist/runtimes/release
	@for x in `ls test`; do echo; printf "\x1b[32;1mRunning test case $$x\x1b[0m\n\n"; $(MAKE) -C test/$$x bench-instrumented-release.dump run-bench-instrumented-release; done

debug-test: dist/runtimes/debug
	@for x in `ls test`; do echo; printf "\x1b[32;1mRunning test case $$x\x1b[0m\n\n"; $(MAKE) -C test/$$x run-instrumented-debug; done

debug-bench-test: dist/runtimes/debug
	@for x in `ls test`; do echo; printf "\x1b[32;1mRunning test case $$x\x1b[0m\n\n"; $(MAKE) -C test/$$x bench-instrumented-debug.dump run-bench-instrumented-debug; done


dist-clean:
	rm -rf dist*
	${MAKE} -C runtimes clean

test-clean:
	for x in `ls test`; do $(MAKE) -C test/$$x clean; done

llvm-clean:
	rm -rf llvm-build llvm-root

runtimes-clean:
	${MAKE} -C ./runtimes clean

clean: dist-clean test-clean runtimes-clean
	stack clean

really-clean: clean
	rm -rf llvm-patches llvm-build runtimes-build
	cd llvm-project && git reset --hard HEAD
