.PHONY: clean install check lint
SHELL = bash

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

include mk/compress.mk

DCK_LIB := $(wildcard ./lib/*.dck)
DCK_PRELUDE := $(wildcard ./prelude/*.dck)

check:
	emd lint $(DCK_LIB) $(DCK_PRELUDE)
	emd check $(DCK_LIB) $(DCK_PRELUDE)

lint:
	hlint src bench run ./test
	yamllint stack.yaml
	yamllint .stylish-haskell.yaml
	yamllint .hlint.yaml

docs: man/emd.1 doc/user-guide.pdf docs/index.html

bins: bin/arm-linux-emd \
    bin/arm-linux-emd.lz \
    bin/arm-linux-emd.zst \
    bin/arm-linux-emd.gz \
    bin/arm-linux-emd.bz2 \
    bin/aarch64-linux-emd \
    bin/aarch64-linux-emd.lz \
    bin/aarch64-linux-emd.zst \
    bin/aarch64-linux-emd.bz2 \
    bin/aarch64-linux-emd.gz \
    bin/powerpc64le-linux-emd \
    bin/powerpc64le-linux-emd.lz \
    bin/powerpc64le-linux-emd.zst \
    bin/powerpc64le-linux-emd.gz \
    bin/powerpc64le-linux-emd.bz2 \
    bin/sparc64-linux-emd

bin/sparc64-linux-emd:
	@mkdir -p $(dir $@)
	@cabal build --with-ghc sparc64-linux-gnu-ghc --with-ghc-pkg sparc64-linux-gnu-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'sparc-linux.*emd$$' -t x -p -I); \
	    cp $$BIN $@ ; \
	    sparc64-linux-gnu-strip $@

bin/powerpc64le-linux-emd:
	@mkdir -p $(dir $@)
	@cabal build --with-ghc powerpc64le-linux-gnu-ghc --with-ghc-pkg powerpc64le-linux-gnu-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'ppc64-linux.*emd$$' -t x -p -I); \
	    cp $$BIN $@ ; \
	    powerpc64le-linux-gnu-strip $@

bin/aarch64-linux-emd:
	@mkdir -p $(dir $@)
	@cabal build --with-ghc aarch64-linux-gnu-ghc --with-ghc-pkg aarch64-linux-gnu-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'aarch64-linux.*emd$$' -t x -p -I); \
	    cp $$BIN $@ ; \
	    aarch64-linux-gnu-strip $@

bin/arm-linux-emd:
	@mkdir -p $(dir $@)
	@cabal build --with-ghc arm-linux-gnueabihf-ghc --with-ghc-pkg arm-linux-gnueabihf-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'arm-linux.*emd$$' -t x -p -I); \
	    cp $$BIN $@ ; \
	    arm-linux-gnueabihf-strip $@

clean:
	rm -rf dist-newstyle .stack-work *.svg stack.yaml.lock doc/user-guide.html *.hp *.prof dist *.emdi bin

docs/index.html: doc/user-guide.html
	@mkdir -p $(dir $@)
	cp $< $@

doc/%.html: doc/%.md
	pandoc $< -s -o $@ --toc

doc/%.pdf: doc/%.md
	pandoc $< -s -o $@ --toc

man/emd.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

install: man/emd.1
	@cabal install exe:emd --overwrite-policy=always
	cp man/emd.1 $(HOME)/.local/share/man/man1
