.PHONY: clean install

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
