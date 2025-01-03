default: test

build:
	dune build

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest

all:
	dune build @all

install: all
	dune install

uninstall:
	dune uninstall


deploy: doc
	dune-release tag
	git push --all
	git push --tag
	dune-release

.PHONY: default build clean doc test all install uninstall deploy
