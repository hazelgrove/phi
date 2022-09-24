DOT ?= dot

.PHONY: all
all	: assets/pipeline.svg

assets/%.svg : %.dot
	$(DOT) $^ -Tsvg -o $@
