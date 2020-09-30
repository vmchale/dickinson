.PHONY: install
SHELL = bash

LIB_DIR := $$HOME/.emd

install:
	mkdir -p $$HOME/.local/bin
	mkdir -p $$HOME/.local/share/man/man1
	mkdir -p $$HOME/.local/share/doc/dickinson
	mkdir -p $(LIB_DIR)/lib
	mkdir -p $(LIB_DIR)/prelude
	cp -r lib/* $(LIB_DIR)/lib/
	cp prelude/* $(LIB_DIR)/prelude
	cp bin/emd $$HOME/.local/bin
	cp man/emd.1 $$HOME/.local/share/man/man1
	cp doc/user-guide.pdf $$HOME/.local/share/doc/dickinson
