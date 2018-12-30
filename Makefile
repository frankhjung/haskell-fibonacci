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

.PHONY:	all check style lint tags build test exec install docs setup clean cleanall

build:	tags check
	@stack build --pedantic .

all:	tags check build test docs

check:	lint style

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint $(SRCS)

tags:
	@hasktags --ctags $(SRCS)

test:
	@stack build --test --coverage .

exec:
	@stack exec -- $(TARGET) $(ARGS) +RTS -s -N4

install:
	@stack build --copy-bins --local-bin-path $(HOME)/bin

docs:
	@stack build --haddock

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

