#!/usr/bin/env make

# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

.PHONY:	all bench build check clean cleanall doc exec ghci install lint setup style tags test

TARGET	:= fib
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
ARGS	?= -h
RTSOPTS	?= +RTS -N4

build:
	@stack build --pedantic --no-test --ghc-options='-O2'

all:	check build test bench doc exec

check:	tags lint style

tags:
	@hasktags --ctags --extendedctag $(SRCS)

lint:
	@hlint $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

test:
	@stack test --coverage --test-arguments '$(RTSOPTS)'

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html $(RTSOPTS)'

doc:
	@stack haddock

exec:
	@stack exec -- $(TARGET) $(ARGS) $(RTSOPTS) -s

install:
	@stack install --local-bin-path $(HOME)/bin $(TARGET)

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

clean:
	@cabal clean
	@stack clean
	@$(RM) -rf $(TARGET).tix

cleanall: clean
	@$(RM) -rf .stack-work/ $(TARGET)

ghci:
	@stack ghci --ghci-options -Wno-type-defaults
