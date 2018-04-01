#!/usr/bin/make -f
#
# This is free and unencumbered software released into the public
# domain.
#
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a
# compiled binary, for any purpose, commercial or non-commercial, and
# by any means.

EMACS ?= emacs
SRC := auto-minor-mode.el
OBJ := $(SRC:.el=.elc)
DIR := $(dir $(lastword $(MAKEFILE_LIST)))
TESTS := $(SRC:.el=.test.stamp)

.INTERMEDIATE: $(TESTS)

all: $(OBJ) test

clean:
	rm -f $(OBJ) $(TESTS)

test: $(TESTS)

%.elc: %.el
	$(EMACS) -Q -batch -L $(DIR) -f batch-byte-compile $<

%.test.stamp: %-test.elc %.elc
	$(EMACS) -Q -batch -L $(DIR) -eval "(checkdoc-file \"$*.el\")"
	$(EMACS) -Q -batch -L $(DIR) -l $< -f ert-run-tests-batch-and-exit
	touch $@

.PHONY: all clean test
