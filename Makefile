#!/usr/bin/env make

# build without stack using:
#
#   ghc -Wall -O2 --make $(SRCS) -o $(TARGET)

TARGET	:= fib
SRCS	:= $(wildcard app/*.hs src/*.hs)
ARGS	?= -h

build: $(SRCS) check
	@stack build

.PHONY:	all
all:	cleanall check build tags

.PHONY:	check
check:	lint style

style:	$(SRCS)
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:	$(SRCS)
	@hlint $(SRCS)

tags:	$(SRCS)
	@hasktags --ctags $(SRCS)

.PHONY:	exec
exec:	# Example:  make ARGS="-i 12 -s 12" exec
	@stack exec -- $(TARGET) $(ARGS)

.PHONY:	clean
clean:
	@stack clean

.PHONY:	cleanall
cleanall: clean
	@$(RM) -rf .stack-work/

