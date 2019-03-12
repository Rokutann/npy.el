EMACS ?= emacs
CASK ?= cask

all: test

ert-test: clean-elc delete-playground
	${MAKE} create-playground
	${MAKE} ert-all
	${MAKE} compile
	${MAKE} ert-all
	${MAKE} delete-playground

test: clean-elc delete-playground
	${MAKE} create-playground
	${MAKE} buttercup-all
	${MAKE} compile
	${MAKE} buttercup-all
	${MAKE} delete-playground

buttercup-all:
	$(CASK) exec buttercup test -L . -l test/test-helper.el -l test/npy-buttercup-init.el

buttercup-essence:
	$(CASK) exec buttercup test/essential -L . -l test/test-helper.el -l test/npy-buttercup-init.el

buttercup-additional:
	$(CASK) exec buttercup test/additional -L . -l test/test-helper.el -l test/npy-buttercup-init.el

buttercup-npy-env:
	$(CASK) exec buttercup test/npy-env -L . -l test/test-helper.el -l test/npy-buttercup-init.el

ert-all:
	${CASK} exec ert-runner

essence:
	${CASK} exec ert-runner test/npy-integration-essential-test.el test/npy-scratch-essential-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile npy.el

clean-elc:
	rm -f npy.elc

check-travis: playground
	sh test/check-travis.sh

create-playground:
	sh test/create-playground.sh

delete-playground:
	sh test/delete-playground.sh

.PHONY: all test docs unit
