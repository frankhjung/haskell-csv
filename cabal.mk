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
	@cabal new-build

test:
	@cabal new-test

exec:
	@cat "files/asx.csv" | cabal new-run example -- $(ARGS) +RTS -s

doc:
	@cabal new-haddock

clean:
	@cabal clean

cleanall: clean
	@cabal new-clean
	@$(RM) -rf *.tix dist/ dist-new/
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))

