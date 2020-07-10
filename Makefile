.PHONY: clean install check lint

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:


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

docs: man/emd.1 doc/user-guide.pdf

clean:
	rm -rf dist-newstyle .stack-work *.svg stack.yaml.lock doc/user-guide.html *.hp *.prof dist *.emdi

doc/%.html: doc/%.md
	pandoc $< -s -o $@ --toc

doc/%.pdf: doc/%.md
	pandoc $< -s -o $@ --toc

man/emd.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

install: man/emd.1
	@cabal install exe:emd --overwrite-policy=always
	cp man/emd.1 $(HOME)/.local/share/man/man1
