.PHONY: clean install

clean:
	rm -rf dist-newstyle .stack-work *.svg stack.yaml.lock

docs: man/emd.1

man/emd.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

install:
	cabal install exe:emd --overwrite-policy=always
	cp man/emd.1 ${HOME}/.local/share/man/man1
