#!/usr/bin/env make

.PHONY: build check tags style lint test exec doc setup clean cleanall

SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= '-'

default: check build test

all:	check build test doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint $(SRCS)

build:
	@cabal build

test:
	@cabal test

doc:
	@cabal haddock

exec:
	@cat "files/asx.csv" | cabal run main -- $(ARGS) +RTS -s

setup:
	@cabal update --only-dependencies

clean:
	@cabal clean

cleanall: clean
	@$(RM) -rf *.tix .cabal/ dist/ dist-new/ public/
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))

