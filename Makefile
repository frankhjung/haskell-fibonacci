#!/usr/bin/env make

# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

.PHONY:	all bench build check clean cleanall doc exec ghci install lint setup style tags test

TARGET	:= fib
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
RTSOPTS	?= +RTS -N1

ARGS	?= -f 34

.PHONY: default
default:	check build test exec

all:	check build test doc exec

check:	tags style lint

tags:
	hasktags --ctags --extendedctag $(SRCS)

style:
	stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	hlint --color $(SRCS)

build:
	cabal new-build

test:
	cabal new-test --test-show-details=always

bench:
	cabal new-bench

doc:
	cabal new-haddock --haddock-all --haddock-quickjump --haddock-hyperlink-source

exec:
	cabal exec $(TARGET) -- -h
	cabal exec $(TARGET) -- $(ARGS) $(RTSOPTS) -s

install:
	cabal new-install --installdir=$(HOME)/bin

setup:
	cabal new-update
	cabal new-configure

clean:
	cabal new-clean
