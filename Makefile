.PHONY: clean install

clean:
	rm -rf dist-newstyle .stack-work *.svg stack.yaml.lock

docs: man/emd.1 doc/user-guide.pdf

doc/%.html: doc/%.md
	pandoc $< -s -o $@

doc/%.pdf: doc/%.md
	pandoc $< -s -o $@

man/emd.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

install: man/emd.1
	@cabal install exe:emd --overwrite-policy=always
	cp man/emd.1 $(HOME)/.local/share/man/man1
