#!/usr/bin/env make

# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

ARGS	?= -r 34
RTSOPTS	?= +RTS -N1 -s
SRC	:= $(wildcard *.hs, */*.hs)
TARGET	:= fib
YAML	:= $(shell git ls-files "*.y*ml")

.PHONY: default
default:	format check build test exec

.PHONY: all
all:	format check build test bench doc exec

.PHONY: format
format:
	@echo format ...
	@stylish-haskell --inplace $(SRC)
	@cabal-fmt --inplace Fibonacci.cabal

.PHONY: check
check:	tags lint

.PHONY: tags
tags:
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: lint
lint:
	@echo lint ...
	@cabal check --verbose
	@hlint --cross --color --show $(SRC)
	@yamllint --strict $(YAML)

.PHONY: build
build:
	@echo build ...
	@stack build --pedantic

.PHONY: test
test:
	@echo test ...
	@stack test

.PHONY: bench
bench:
	@echo bench ...
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY: doc
doc:
	@echo doc ...
	@stack haddock

.PHONY: exec
exec:
	-stack exec -- $(TARGET) -h
	-stack exec -- $(TARGET) $(ARGS) $(RTSOPTS)

.PHONY: setup
setup:
	@stack path
	@stack query
	@stack ls dependencies

.PHONY: clean
clean:
	@stack clean
	@cabal clean

.PHONY: cleanall
cleanall: clean
	@stack purge
