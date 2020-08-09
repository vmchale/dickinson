.PHONY: install
SHELL = bash

LIB_DIR := $$HOME/.emd

install:
	mkdir -p $$HOME/.local/bin
	mkdir -p $HOME/.local/share/man/man1
	mkdir -p $(LIB_DIR)/lib
	mkdir -p $(LIB_DIR)/prelude
	cp lib/* $(LIB_DIR)/lib/
	cp prelude/* $(LIB_DIR)/prelude
	cp bin/emd $$HOME/.local/bin
