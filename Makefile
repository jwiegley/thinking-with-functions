NAME = org-beamer-template

PYTHON	 = /usr/bin/python
PRESENT	 = /Applications/Misc/Présentation.app/Contents/MacOS/presentation.py
PDF	 = $(NAME).pdf
EMACS    = emacs

all: $(PDF)

open: $(PDF)
	open $<

present: all
	$(PYTHON) $(PRESENT) $(PDF)

org-support-code.cabal: package.yaml
	hpack

# Ensure all examples work before building the slide deck
%.tex: %.org Makefile Support.hs org-support-code.cabal
	cabal build
	./dist/build/org-support-code/org-support-code
	$(EMACS) -batch -L . -l support \
	    --eval="(progn (find-file \"$<\") (extract-code-blocks) (setq org-export-latex-minted-options '((\"fontsize\" \"\\\\small\") (\"linenos\" \"true\"))) (org-beamer-export-to-latex))"

%.pdf: %.tex
	pdflatex -shell-escape -interaction nonstopmode $<
	pdflatex -shell-escape -interaction nonstopmode $<
	pdflatex -shell-escape -interaction nonstopmode $<

clean:
	rm -fr html
	rm -f *.tex *.pdf *.vrb *.aux *.log *.nav *.out *.snm *.toc *.upa
	rm -f src/*.d src/*.vo src/*.glob
