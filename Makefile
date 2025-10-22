.PHONY: all clean

all: _build/motivation.pdf _build/motivation.html

clean:
	rm -rf _build

_build/motivation.pdf: doc/motivation.md
	mkdir -p _build
	pandoc doc/motivation.md -o _build/motivation.pdf

_build/motivation.html: doc/motivation.md
	mkdir -p _build
	pandoc doc/motivation.md -o _build/motivation.html --highlight-style=pygments
