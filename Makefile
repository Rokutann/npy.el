EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc delete-playground
	${MAKE} playground
	${MAKE} unit
#	${MAKE} compile
#	${MAKE} unit
#	${MAKE} clean-elc
	${MAKE} delete-playground

check-travis: playground
	sh test/check-travis.sh

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile npy.el

clean-elc:
	rm -f npy.elc

playground:
	sh test/create-playground.sh

delete-playground:
	sh test/delete-playground.sh

.PHONY: all test docs unit
