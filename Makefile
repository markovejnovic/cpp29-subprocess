.PHONY: all clean html watch

all: pdf html

clean:
	rm -rf _build

pdf: _build/motivation.pdf
html: _build/html/motivation.html _build/html/index.html

_build/html/index.html: _build/_artifacts/index.md
	pandoc $< -o $@ --highlight-style=pygments

_build/motivation.pdf: doc/motivation.md
	mkdir -p $(dir $@)
	pandoc $< -o $@

_build/html/motivation.html: doc/motivation.md
	mkdir -p $(dir $@)
	pandoc $< -o $@ --highlight-style=pygments

_build/_artifacts/index.md: doc/index.mdlisp tools/md-proc.scm
	mkdir -p $(dir $@)
	guile ./tools/md-proc.scm -- $< > $@
