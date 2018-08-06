#!/usr/bin/env make

# build
#	@ghc -Wall -O2 --make $(SRCS)
# run
#	@./$(TARGET)
#	@stack exec -- $(TARGET)

TARGET	:= csv
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

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
	@stack exec $(TARGET)

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
