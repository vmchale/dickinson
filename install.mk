.PHONY: install
SHELL = bash

LIB_DIR := $(shell ./bin/emd dir)

install:
	mkdir -p $$HOME/.local/bin
	mkdir -p $(LIB_DIR)/share/man
	mkdir -p $(LIB_DIR)/lib
	mkdir -p $(LIB_DIR)/prelude
	cp lib/* $(LIB_DIR)/lib/
	cp prelude/* $(LIB_DIR)/prelude
	cp bin/emd $$HOME/.local/bin
