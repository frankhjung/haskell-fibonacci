#!/usr/bin/env make

# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

TARGET	:= fib
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
ARGS	?= -h
RTSOPTS	?= +RTS -N4

.PHONY:	all bench build check clean cleanall cover docs exec install lint setup style tags test

build:
	@stack build --pedantic --no-test --ghc-options='-O2'

all:	check build cover test bench docs exec

check:	tags lint style

tags:
	@hasktags --ctags $(SRCS)

lint:
	@hlint $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

cover:
	@stack build --no-test --coverage .

test:
	@stack test --test-arguments '$(RTSOPTS)'

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html $(RTSOPTS)'

docs:
	@stack build --haddock

exec:
	@stack exec -- $(TARGET) $(ARGS) $(RTSOPTS) -s

install:
	@stack build --copy-bins --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

clean:
	@cabal clean
	@stack clean
	@rm -rf $(TARGET).tix

cleanall: clean
	@$(RM) -rf .stack-work/ $(TARGET)

