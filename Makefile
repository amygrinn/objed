# make EMACS=$HOME/emacs/<version>/bin/emacs
EMACS = emacs

update:
	$(EMACS) -Q -batch -l test/make-update.el

lint:
	$(EMACS) -Q -batch -l test/elpa.el -l test/make-lint.el

compile: clean
	$(EMACS) -Q -batch -l test/deps.el -l test/elpa.el -l test/make-compile.el

test: compile
	$(EMACS) -Q -batch -l test/elpa.el -l test/make-test.el

package:
	$(EMACS) -Q -batch -l test/elpa.el -l test/make-package.el

clean:
	rm -f *.elc
	rm -f test/*.elc	


.PHONY: update lint compile test clean
