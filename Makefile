#!/usr/bin/env make

# build without stack using:
#
#   ghc -Wall -O2 --make $(SRCS) -o $(TARGET)

TARGET = fib
SRCS = $(wildcard *.hs */*.hs)

build: $(SRCS) check
	@stack build

all:	cleanall check build tags

check:	lint style

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint $(SRCS)

tags:	$(SRCS)
	@hasktags --ctags $(SRCS)

exec:	# Example:  make ARGS="-i 12 -s 12" exec
	@stack exec -- $(TARGET) $(ARGS)

clean:
	@stack clean

cleanall: clean
	@$(RM) -rf .stack-work/

