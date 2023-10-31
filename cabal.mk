#!/usr/bin/env make

.DEFAULT_GOAL := default

SRCS	:= $(shell git ls-files | grep --perl \.hs)
YAML	:= $(shell git ls-files | grep --perl \.y?ml)

.PHONY: default
default: format check build test exec

.PHONY: all
all:	format check build test doc exec

.PHONY: format
format:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)
	@cabal-fmt --inline csv.cabal

.PHONY: check
check:	tags lint

.PHONY: tags
tags: $(SRCS)
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY: lint
lint:
	@cabal check
	@hlint --cross --color --show $(SRCS)
	@yamllint --strict $(YAML)

.PHONY: build
build:
	@cabal build

.PHONY: test
test:
	@cabal test

.PHONY: doc
doc:
	@cabal haddock

.PHONY: exec
exec:
	@cat "files/asx.csv" | cabal run main -- - +RTS -s

.PHONY: setup
setup:
	@cabal update --only-dependencies

.PHONY: clean
clean:
	@cabal clean

cleanall: clean
	@$(RM) -rf *.tix .cabal/ dist/ dist-new/ public/
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))
