.PHONY: clean

clean:
	rm -rf dist-newstyle .stack-work *.svg stack.yaml.lock

docs: man/emd.1

man/emd.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@
