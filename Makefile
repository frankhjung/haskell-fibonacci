#!/usr/bin/env make

# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

ARGS	?= -r 34
RTSOPTS	?= +RTS -N1 -s
SRC	:= $(wildcard *.hs, */*.hs)
TARGET	:= fib
YAML	:= $(shell git ls-files | grep --perl \.y?ml)

.PHONY: default
default:	check build test exec

.PHONY: all
all:	check build test bench doc exec

.PHONY: check
check:	tags style lint

.PHONY: tags
tags:
	hasktags --ctags --extendedctag $(SRC)

.PHONY: style
style:
	stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRC)

.PHONY: lint
lint:
	@cabal check --verbose
	@hlint --cross --color --show $(SRC)
	@yamllint --strict $(YAML)

.PHONY: build
build:
	@stack build --pedantic

.PHONY: test
test:
	@stack test

.PHONY: bench
bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY: doc
doc:
	@stack haddock --no-haddock-deps

.PHONY: exec
exec:
	-stack exec -- $(TARGET) -h
	-stack exec -- $(TARGET) $(ARGS) $(RTSOPTS)

.PHONY: setup
setup:
	stack path
	stack query
	stack ls dependencies

.PHONY: clean
clean:
	@stack clean
	@cabal clean

.PHONY: cleanall
cleanall: clean
	@stack clean --full
