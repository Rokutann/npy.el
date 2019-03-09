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

essence:
	${CASK} exec ert-runner test/npy-integration-essential-test.el test/npy-scratch-essential-test.el

integration:
	${CASK} exec ert-runner test/npy-integration-test.el

buttercup-essence:
	$(CASK) exec buttercup test/essential -l test/test-helper.el

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
