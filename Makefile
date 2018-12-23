#!/usr/bin/env make

# build without stack using:
#
#   ghc -Wall -O2 --make $(SRCS) -o $(TARGET)

TARGET	:= fib
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
ARGS	?= -h

.PHONY:	all check style lint tags build test exec install docs setup clean cleanall

build:	tags check
	@stack build --pedantic --no-test

all:	tags check build test docs

check:	lint style

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint $(SRCS)

tags:
	@hasktags --ctags $(SRCS)

test:
	@stack test --coverage

exec:
	# Example:  make ARGS="-i 12 -s 12" exec
	@stack exec -- $(TARGET) $(ARGS)

install:
	@stack install --local-bin-path $(HOME)/bin

docs:
	@stack haddock

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack list-dependencies

clean:
	@cabal clean
	@stack clean

cleanall: clean
	@$(RM) -rf .stack-work/ $(TARGET) $(TARGET).tix

