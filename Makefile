#!/usr/bin/env make

# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

.PHONY:	all bench build check clean cleanall doc exec ghci install lint setup style tags test

TARGET	:= fib
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
RTSOPTS	?= +RTS -N1

ARGS	?= -h

.PHONY: default
default:	check build test exec

all:	check build test doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint --color $(SRCS)

build:
	@stack build --pedantic --no-test --ghc-options='-O2'

test:
	@stack test

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html $(RTSOPTS)'

doc:
	@stack test --coverage --no-run-tests --test-arguments '$(RTSOPTS)'
	@stack haddock

exec:
	@stack exec $(TARGET) -- $(ARGS) $(RTSOPTS) -s

install:
	@stack install --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only
	-stack query
	-stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

jupyter:
	@stack exec jupyter -- notebook

clean:
	@stack clean
	@$(RM) -rf $(TARGET).tix

cleanall: clean
	@stack clean --full
	# @$(RM) -rf .stack-work/ $(TARGET)
