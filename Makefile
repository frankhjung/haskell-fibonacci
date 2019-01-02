#!/usr/bin/env make

# build without stack using:
#
#   ghc -Wall -O2 --make $(SRCS) -o $(TARGET)
#
# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

TARGET	:= fib
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
ARGS	?= -h

.PHONY:	all check style lint tags build cover test exec install docs setup clean cleanall

build:	tags check
	@stack build --pedantic --no-test

all:	tags check build cover test docs

tags:
	@hasktags --ctags $(SRCS)

check:	lint style

lint:
	@hlint $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

cover:
	@stack build --no-test --coverage .

test:
	@stack build --test

docs:
	@stack build --haddock

exec:
	@stack exec -- $(TARGET) $(ARGS) +RTS -s -N4

install:
	@stack build --copy-bins --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack list-dependencies

clean:
	@cabal clean
	@stack clean
	@rm -rf $(TARGET).tix

cleanall: clean
	@$(RM) -rf .stack-work/ $(TARGET)

