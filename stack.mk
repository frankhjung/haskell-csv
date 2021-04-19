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
	@stack build --pedantic --no-test

test:
	@stack test --coverage

exec:
	@cat "files/asx.csv" | stack exec example
	@#cat "files/asx.csv" | stack exec example -- $(ARGS) +RTS -s

doc:
	@stack haddock

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	#stack ls dependencies

clean:
	@stack clean
	@$(RM) -rf *.tix stack.yaml.lock

cleanall: clean
	@$(RM) -rf tags .stack-work/ dist-*/
