#!/usr/bin/env make

TARGET = fib
SRCS = $(wildcard */*.hs)

build: ${SRCS} check
	-ghc -Wall -O2 --make ${SRCS} -o ${TARGET}

all:	cleanall check tags build

check:	lint style

style:
	-stylish-haskell --config=.stylish-haskell.yaml --inplace ${SRCS}

lint:
	-hlint ${SRCS}

tags:
	@hasktags --ctags ${SRCS}

clean:
	@$(RM) */*.o */*.hi */*.dyn_hi */*.dyn_o

cleanall: clean
	@$(RM) $(TARGET)

