EMACS =		emacs

compile: vc-got.elc vc-got-stage.elc

clean:
	rm -f *.elc

.SUFFIXES: .el .elc
.el.elc:
	${EMACS} -Q --batch -L . -f batch-byte-compile $<
