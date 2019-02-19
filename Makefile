EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc delete-playground
	${MAKE} playground
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc
	${MAKE} delete-playground

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile nalist.el

clean-elc:
	rm -f npy.elc

playground:
	sh test/create-playground.sh

delete-playground:
	sh test/delete-playground.sh

.PHONY: all test docs unit
