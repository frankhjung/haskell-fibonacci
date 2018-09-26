#!/usr/bin/env make

# build without stack using:
#
#   ghc -Wall -O2 --make $(SRCS) -o $(TARGET)

TARGET	:= fib
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
ARGS	?= -h

.PHONY:	all
all:	tags check build test docs

check:	lint style

style:	$(SRCS)
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:	$(SRCS)
	@hlint $(SRCS)

tags:	$(SRCS)
	@hasktags --ctags $(SRCS)

build:	$(SRCS)
	@stack build

.PHONY:	test
test:
	@stack test

.PHONY:	exec
exec:	# Example:  make ARGS="-i 12 -s 12" exec
	@stack exec -- $(TARGET) $(ARGS)

.PHONY: install
install:
	@stack install --local-bin-path $(HOME)/bin

.PHONY:	docs
docs:
	@stack haddock

.PHONY:	setup
setup:
	-stack setup
	-stack query
	-stack ls dependencies

.PHONY:	clean
clean:
	@cabal clean
	@stack clean

.PHONY:	cleanall
cleanall: clean
	@$(RM) -rf .stack-work/

