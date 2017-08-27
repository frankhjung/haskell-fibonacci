# Haskell

This was one of my first projects, before I discovered Cabal and Stack.

It is the "Hello World" equivalent for recursion:

## Fibonacci

### Usage

```bash
usage: fib [options]
  [-h,--help]            Help
  [-i,--index <int>]     Generate Fibonacci at index
  [-s,--sequence <int>]  Generate Fibonacci sequence
```

### Performance

Measure elapsed CPU user time with:

```bash
elapsed="$( TIMEFORMAT='%lU';time ( ./main -s 120 ) 2>&1 1>/dev/null )"
echo $elapsed
```

To profile program in interactive GHC session:

```c2hs
# start session
$ ghci app/main.hs src/fib.hs

# now run commands
Prelude>  set +s
*Main>    :main -i 110
```

#### Example

```
$ ghci app/main.hs src/fib.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/frank/.ghci
[1 of 2] Compiling Fibonacci        ( src/fib.hs, interpreted )
[2 of 2] Compiling Main             ( app/main.hs, interpreted )
Ok, modules loaded: Fibonacci, Main.
*Main > :set +s
*Main > :main -i 110
Fibonacci at index is 43566776258854844738105
(0.02 secs, 169,352 bytes)
```

## Project

A simple project to capture what I learn about Haskell.
As a guide I am using "A Gentle Introduction to Haskell, Version 98".

On Debian, install the following (and there dependencies):

* haskell-doc
* haskell98-report
* haskell98-tutorial
* haskell-platform

## Resources

* http://dev.stephendiehl.com/hask/
* https://haskell-lang.org/
* https://wiki.debian.org/Haskell
* https://wiki.haskell.org/Haskell
* https://www.haskell.org/
* https://www.haskell.org/hoogle
* http://www.cis.upenn.edu/~cis194/spring13/lectures.html

## Packages

From parsing command line arguments:

* libghc-parseargs-dev

To colourise Haskell code use HsColour command from:

* hscolour

## Interactive

* http://dev.stephendiehl.com/hask/#ghci

Invoke interactive Haskell session for Glasgow Haskell Compiler:

```bash
ghci
```

### browse module

Example: browse contents of Data.List module:

```bash
:browse Data.List

:module Data.List
:browse!
```

## References

### System F

* https://en.wikipedia.org/wiki/System_F

### statistics

* http://dev.stephendiehl.com/hask/#statistics

### mathematics

* https://idontgetoutmuch.wordpress.com/category/haskell/
