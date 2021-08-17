EMACS =		emacs

.PHONY: all compile lint clean

all: compile lint

compile: vc-got.elc

lint:
	${EMACS} -Q --batch -L . -l targets/lint.el

clean:
	rm -f *.elc

.SUFFIXES: .el .elc
.el.elc:
	${EMACS} -Q --batch -L . -f batch-byte-compile $<
