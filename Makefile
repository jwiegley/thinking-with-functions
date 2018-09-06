NAME    = denotational-design
PYTHON	= /usr/bin/python
PRESENT	= /Applications/Misc/Présentation.app/Contents/MacOS/presentation.py
PDF	= $(NAME).pdf
EMACS   = emacs

all: $(PDF)

open: $(PDF)
	open $<

present: all
	$(PYTHON) $(PRESENT) $(PDF)

# Ensure all examples work before building the slide deck
%.tex: %.org Makefile
	$(EMACS) --debug-init -batch $(EMACS_ARGS) -L . -l support -f perform-extraction $<

%.pdf: %.tex
	xelatex -8bit -shell-escape -interaction nonstopmode $<
	xelatex -8bit -shell-escape -interaction nonstopmode $<
	xelatex -8bit -shell-escape -interaction nonstopmode $<

clean:
	rm -fr html
	rm -f *.tex *.pdf *.vrb *.aux *.log *.nav *.out *.snm *.toc *.upa
	rm -f src/*.d src/*.vo src/*.glob
	rm -fr _minted-* auto diagram*.svg svg-inkscape *.cabal dist

watch:
	fswatch --batch-marker --latency 2 -m poll_monitor \
	    *.hs *.el *.org *.yaml Makefile \
	    | while read event; do \
	          [[ $$event == NoOp ]] && PATH=$(PWD):$(PATH) make; \
	      done
