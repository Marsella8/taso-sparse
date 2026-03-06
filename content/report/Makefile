RAWDOTS := $(shell find ./res/dot -name '*.dot')
DOTS := $(RAWDOTS:%.dot=%.dot.tex)

.PHONY: pdf watch clean

pdf: main.pdf

%.dot.tex: %.dot
	dot2tex --figonly -ftikz -o $@ $<

main.pdf: main.tex $(wildcard *.bib) $(DOTS)
	tectonic $<

watch:
	find . -type f \( -name '*.tex' -o -name '*.bib' -o -name '*.dot' \) ! -name '*.dot.tex' | entr -c make pdf

clean:
	rm -f main.pdf $(DOTS)
