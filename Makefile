#!/usr/bin/env make

.DEFAULT_GOAL := default

SRCS	:= $(shell git ls-files | grep --perl \.hs)
YAML	:= $(shell git ls-files | grep --perl \.y?ml)
TARGET	:= csv

.PHONY: default
default: format check build test exec

.PHONY: all
all:	format check build test doc exec

.PHONY: format
format:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)
	@cabal-fmt --inplace $(TARGET).cabal

.PHONY: check
check:	tags lint

.PHONY: tags
tags:
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY: lint
lint:
	@cabal check
	@hlint --cross --color --show $(SRCS)
	@yamllint --strict $(YAML)

.PHONY: build
build:
	@stack build --pedantic --fast

.PHONY: test
test:
	@stack test --fast

.PHONY: doc
doc:
	@stack haddock

.PHONY: exec
exec:
	@stack exec -- main - +RTS -s < data/quotes.csv

.PHONY: setup
setup:
	stack path
	stack query
	stack ls dependencies

.PHONY: clean
clean:
	@stack clean

.PHONY: cleanall
cleanall: clean
	@stack purge
	@rm -f stack.yaml.lock
