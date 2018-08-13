#!/usr/bin/env make

TARGET	:= csv
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= '-'

all:	check build test exec

check:	style lint tags

style:	$(SRCS)
	-stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	-hlint $(SRCS) --git --color --show

tags:	$(SRCS)
	-hasktags --ctags --extendedctag $(SRCS)

build:	$(SRCS)
	@stack build

test:	$(SRCS)
	@stack test

exec:	build
	@cat "files/asx.csv" | stack exec $(TARGET) -- $(ARGS)

bench:	build
	@stack bench

doc:	build
	@stack haddock

install:
	@stack install --local-bin-path $(HOME)/bin $(TARGET)

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@$(RM) -rf dist

cleanall: clean
	@$(RM) -rf .stack-work/
