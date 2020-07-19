.PHONY: clean install check lint release
SHELL = bash

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

include mk/compress.mk

DCK_LIB := $(wildcard ./lib/*.dck)
DCK_PRELUDE := $(wildcard ./prelude/*.dck)

HS_SRC := $(shell find run -type f) $(shell find src -type f) language-dickinson.cabal

VERSION := $(shell grep -P -o '\d+\.\d+\.\d+\.\d+' language-dickinson.cabal | head -n1)

TOKEN := $(shell cat ~/.git-token)

GR_OPTIONS := -s $(TOKEN) -u vmchale -r dickinson -t $(VERSION)

DISTBINS := bin/arm-linux-emd.lz \
    bin/arm-linux-emd.zst \
    bin/arm-linux-emd.gz \
    bin/arm-linux-emd.bz2 \
    bin/aarch64-linux-emd.lz \
    bin/aarch64-linux-emd.zst \
    bin/aarch64-linux-emd.bz2 \
    bin/aarch64-linux-emd.gz \
    bin/powerpc64le-linux-emd.lz \
    bin/powerpc64le-linux-emd.zst \
    bin/powerpc64le-linux-emd.gz \
    bin/powerpc64le-linux-emd.bz2 \
    bin/sparc64-linux-emd.lz \
    bin/sparc64-linux-emd.zst \
    bin/sparc64-linux-emd.gz \
    bin/sparc64-linux-emd.bz2 \
    bin/x86_64-linux-emd.lz \
    bin/x86_64-linux-emd.zst \
    bin/x86_64-linux-emd.gz \
    bin/x86_64-linux-emd.bz2

check:
	emd lint $(DCK_LIB) $(DCK_PRELUDE)
	emd check $(DCK_LIB) $(DCK_PRELUDE)

release: man/emd.1 distbins
	# github-release release $(GR_OPTIONS)
	github-release upload $(GR_OPTIONS) -n emd.1 -f man/emd.1 --replace
	for bin in $(DISTBINS) ; do \
	    github-release upload $(GR_OPTIONS) -n $$(basename $$bin) -f $$bin --replace ; \
	done

lint:
	hlint src bench run ./test
	yamllint stack.yaml
	yamllint .stylish-haskell.yaml
	yamllint .hlint.yaml

docs: man/emd.1 doc/user-guide.pdf docs/index.html

distbins: $(DISTBINS)

bins: bin/arm-linux-emd \
    bin/aarch64-linux-emd \
    bin/powerpc64le-linux-emd \
    bin/sparc64-linux-emd \
    bin/x86_64-linux-emd

# might be slower b/c static but
bin/x86_64-linux-emd: $(HS_SRC)
	@mkdir -p $(dir $@)
	cabal build exe:emd --enable-executable-static
	export BIN=$$(fd 'x86_64-linux.*emd$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    strip $@

bin/sparc64-linux-emd: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc sparc64-linux-gnu-ghc --with-ghc-pkg sparc64-linux-gnu-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'sparc-linux.*emd$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    sparc64-linux-gnu-strip $@

bin/powerpc64le-linux-emd: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc powerpc64le-linux-gnu-ghc --with-ghc-pkg powerpc64le-linux-gnu-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'ppc64-linux.*emd$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    powerpc64le-linux-gnu-strip $@

bin/aarch64-linux-emd: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc aarch64-linux-gnu-ghc --with-ghc-pkg aarch64-linux-gnu-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'aarch64-linux.*emd$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    aarch64-linux-gnu-strip $@

bin/arm-linux-emd: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc arm-linux-gnueabihf-ghc --with-ghc-pkg arm-linux-gnueabihf-ghc-pkg --constraint='language-dickinson +cross' exe:emd --enable-executable-static
	export BIN=$$(fd 'arm-linux.*emd$$' dist-newstyle -t x -p -I); \
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
