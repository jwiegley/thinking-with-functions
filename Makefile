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

dist/build/org-support-code/org-support-code: Main.hs org-support-code.cabal
	cabal build

# Ensure all examples work before building the slide deck
%.tex: %.org Makefile dist/build/org-support-code/org-support-code
	$(EMACS) --debug-init -batch -L . -l support -f perform-extraction $<

%.pdf: %.tex
	xelatex -shell-escape -interaction nonstopmode $<
	xelatex -shell-escape -interaction nonstopmode $<
	xelatex -shell-escape -interaction nonstopmode $<

clean:
	rm -fr html
	rm -f *.tex *.pdf *.vrb *.aux *.log *.nav *.out *.snm *.toc *.upa
	rm -f src/*.d src/*.vo src/*.glob

watch:
	fswatch --batch-marker --latency 2 -m poll_monitor \
	    *.hs *.el *.org *.yaml Makefile \
	    | while read event; do \
	          [[ $$event == NoOp ]] && PATH=$(PWD):$(PATH) make; \
	      done
