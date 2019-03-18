EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc delete-playground
	${MAKE} create-playground
	${MAKE} buttercup-all
	${MAKE} compile
	${MAKE} buttercup-all
	${MAKE} delete-playground

buttercup-all:
	$(CASK) exec buttercup test -L . -l test/test-helper.el -l test/npy-buttercup-init.el

essential:
	$(CASK) exec buttercup test/essential -L . -l test/test-helper.el -l test/npy-buttercup-init.el

additional:
	$(CASK) exec buttercup test/additional -L . -l test/test-helper.el -l test/npy-buttercup-init.el

npy-env:
	$(CASK) exec buttercup test/npy-env -L . -l test/test-helper.el -l test/npy-buttercup-init.el

npy-dispatch-feature:
	$(CASK) exec buttercup test/npy-dispatch-feature -L . -l test/test-helper.el -l test/npy-buttercup-init.el

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
