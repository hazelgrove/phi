DOT ?= dot

.PHONY: all
all	: assets/pipeline.svg assets/hole-contexts.svg

assets/%.svg : %.dot
	$(DOT) $^ -Tsvg -o $@
