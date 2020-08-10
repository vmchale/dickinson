.PHONY: clean install check lint release
SHELL = bash

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

include mk/compress.mk

DCK_LIB := $(wildcard ./lib/*.dck)
DCK_PRELUDE := $(wildcard ./prelude/*.dck)

HS_SRC := $(shell find run -type f) $(shell find src -type f) language-dickinson.cabal

VERSION := $(shell grep -o '[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*' language-dickinson.cabal | head -n1)

GR_OPTIONS := -u vmchale -r dickinson -t $(VERSION)

DOCS := man/emd.1 doc/user-guide.pdf

DISTS := x86_64-linux-dist.tar.lz \
    x86_64-linux-dist.tar.zst \
    x86_64-linux-dist.tar.bz2 \
    x86_64-linux-dist.tar.gz \
    arm-linux-dist.tar.lz \
    arm-linux-dist.tar.zst \
    arm-linux-dist.tar.bz2 \
    arm-linux-dist.tar.gz \
    aarch64-linux-dist.tar.lz \
    aarch64-linux-dist.tar.zst \
    aarch64-linux-dist.tar.bz2 \
    aarch64-linux-dist.tar.gz \
    sparc64-linux-dist.tar.lz \
    sparc64-linux-dist.tar.zst \
    sparc64-linux-dist.tar.bz2 \
    sparc64-linux-dist.tar.gz \
    powerpc64le-linux-dist.tar.lz \
    powerpc64le-linux-dist.tar.zst \
    powerpc64le-linux-dist.tar.bz2 \
    powerpc64le-linux-dist.tar.gz \
    x86_64-freebsd-dist.tar.lz \
    x86_64-freebsd-dist.tar.zst \
    x86_64-freebsd-dist.tar.bz2 \
    x86_64-freebsd-dist.tar.gz

check:
	emd lint $(DCK_LIB) $(DCK_PRELUDE)
	emd check $(DCK_LIB) $(DCK_PRELUDE)

release: dists
	github-release release $(GR_OPTIONS)
	for dist in $(DISTS) ; do \
	    github-release upload $(GR_OPTIONS) -n $$dist -f $$dist --replace ; \
	done

lint:
	hlint src bench run ./test
	yamllint stack.yaml
	yamllint .stylish-haskell.yaml
	yamllint .hlint.yaml

docs: man/emd.1 doc/user-guide.pdf docs/index.html

dists: $(DISTS)

bins: bin/x86_64-linux-emd \
    bin/arm-linux-emd \
    bin/aarch64-linux-emd \
    bin/powerpc64le-linux-emd \
    bin/sparc64-linux-emd \
    bin/x86_64-freebsd-emd

%-dist.tar: bin/%-emd $(DCK_PRELUDE) $(DCK_LIB) $(DOCS) install.mk
	star -c \
	    -s ,$<,language-dickinson-$(VERSION)/bin/emd, \
	    -s ,install.mk,language-dickinson-$(VERSION)/Makefile, \
	    -s ,^,language-dickinson-$(VERSION)/, \
	    -f $@ $^

language-dickinson-$(VERSION).tar: $(DCK_PRELUDE) $(DCK_LIB) $(HS_SRC)
	cabal sdist --list-only | spax -w -s ,^,language-dickinson-$(VERSION)/, -f $@

bin/x86_64-freebsd-emd:
	@mkdir -p $(dir $@)
	vagrant ssh --command 'set -x \
	    cabal update \
	    cabal unpack language-dickinson \
	    cd language-dickinson-$(VERSION) \
	    cabal build exe:emd --constraint="language-dickinson -zstd" --enable-executable-static \
	    bin=$$(find . -name emd -type f) \
	    strip $$bin \
	    cp $$bin /vagrant/$$(basename $@)'
	mv $$(basename $@) $@

bin/x86_64-darwin-emd: $(HS_SRC)
	@mkdir -p $(dir $@)
	cabal build exe:emd
	export BIN=$$(fd 'x86_64-osx.*emd$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    strip $@

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
	rm -rf dist-newstyle .stack-work *.svg stack.yaml.lock doc/user-guide.html *.hp *.prof dist *.emdi bin *.pax* *.tar*

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
