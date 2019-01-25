#!/usr/bin/env make

.PHONY: build check tags style lint test exec bench doc install setup jupyter ghci clean cleanall

TARGET	:= csv
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= '-'

build:
	@stack build --pedantic --no-test --ghc-options='-O2'

all:	check build test doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint $(SRCS)

test:
	@stack test --coverage

exec:
	@cat "files/asx.csv" | stack exec $(TARGET) -- $(ARGS) +RTS -s

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

doc:
	@stack haddock

install:
	@stack install --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

jupyter:
	@stack exec jupyter -- notebook

clean:
	@stack clean
	@$(RM) -rf $(TARGET).tix

cleanall: clean
	@$(RM) -rf .stack-work/
