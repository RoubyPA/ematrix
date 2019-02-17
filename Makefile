EMACS = emacs -Q -q --batch -nw --eval "(package-initialize)"
EMACS_COMPILE = -f emacs-lisp-byte-compile
EMACS_DIR = ~/.emacs.d/matrix/

all: matrix.elc

%.elc: %.el
	$(info ELC $@)
	@$(EMACS) $< $(EMACS_COMPILE)

install: matrix.elc
	$(info Install)
	@mkdir -p $(EMACS_DIR)
	@cp -v *.el  $(EMACS_DIR)
	@cp -v *.elc $(EMACS_DIR)

.PHONY: clean
clean:
	rm -v *.elc
