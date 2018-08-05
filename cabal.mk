#!/usr/bin/env make

TARGET	:= csv
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

ARGS	?= -h

all:	check build test

check:	style lint tags
	@cabal check

style:	$(SRCS)
	-stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	-hlint --color $(SRCS)

tags:	$(SRCS)
	-hasktags --ctags $(SRCS)

build:	$(SRCS)
	@cabal build

test:	$(SRCS)
	@cabal test

exec:	build
	@cabal exec $(TARGET) -- $(ARGS)

run:	build
	@cabal run $(TARGET) -- $(ARGS)

copy:	build
	@cabal copy

ghci:
	-ghci -Wno-type-defaults

clean:
	@cabal clean

cleanall: clean
	@$(RM) -rf ./dist
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))
