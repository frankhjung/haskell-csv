#!/usr/bin/env make

TARGET	:= csv
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= "files/asx.csv"

.PHONY: all
all:	check build test docs run

.PHONY: check
check:	style tags lint
	@cabal check

style:	$(SRCS)
	-stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	-hlint --color $(SRCS)

tags:	$(SRCS)
	-hasktags --ctags $(SRCS)

build:	$(SRCS)
	@cabal build

.PHONY: test
test:
	@cabal test

.PHONY: doc
docs:
	@cabal doctest
	@cabal haddock

.PHONY: run
run:
	@cabal run $(TARGET) -- $(ARGS)

.PHONY: copy
copy:
	@cabal copy

.PHONY: ghci
ghci:
	-ghci -Wno-type-defaults

.PHONY: clean
clean:
	@cabal clean
	@stack clean

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf ./dist
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))
	@$(RM) -rf .stack-work/
