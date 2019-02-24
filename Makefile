EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc delete-playground
	${MAKE} create-playground
	${MAKE} ert-all
	${MAKE} compile
	${MAKE} ert-all
	${MAKE} delete-playground

check-travis: playground
	sh test/check-travis.sh

ert-all:
	${CASK} exec ert-runner

integration:
	${CASK} exec ert-runner test/npy-integration-test.el

npy-scratch:
	${CASK} exec ert-runner test/npy-scratch-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile npy.el

clean-elc:
	rm -f npy.elc

create-playground:
	sh test/create-playground.sh

delete-playground:
	sh test/delete-playground.sh

.PHONY: all test docs unit
