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
	@cat "files/asx.csv" | cabal new-run main -- $(ARGS) +RTS -s

doc:
	@cabal new-haddock

setup:
	@cabal new-update --only-dependencies

clean:
	@cabal clean

cleanall: clean
	@$(RM) -rf *.tix .cabal/ dist/ dist-new/ public/
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))
	@cabal new-clean

