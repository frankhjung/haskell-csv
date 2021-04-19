#!/usr/bin/env make

.PHONY: build check tags style lint test exec bench doc install setup jupyter ghci clean cleanall

TARGET	:= quotescsv
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
	@cat "files/asx.csv" | cabal new-run $(TARGET) -- $(ARGS) +RTS -s

doc:
	@cabal new-haddock

# install:
# 	@cabal new-install --installdir=$(HOME)/bin --install-method=copy

clean:
	@cabal clean

cleanall: clean
	@cabal new-clean
	@$(RM) -rf $(TARGET).tix stack.yaml.lock dist/
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))

