LATEXMK ?= latexmk
DOT ?= dot

.PHONY: all
all	: index.pdf assets/pipeline.svg

%.pdf : %.tex
	$(LATEXMK) $*.tex
	mv target/$*.pdf $*.pdf

assets/%.svg : %.dot
	$(DOT) $^ -Tsvg -o $@

.PHONY : clean
clean :
	rm -rf target
	rm -f *.pdf
