# Haskell

Simple project to calculate Fibonacci sequences.

## Usage

```bash
usage: fib [options]
  [-h,--help]            Help
  [-i,--index <int>]     Generate Fibonacci at index (>0)
  [-s,--sequence <int>]  Generate Fibonacci sequence (>0)
  [-p,--parallel <int>]  Generate Fibonacci value in parallel (>0)
```

## Example

```bash
echo "$( TIMEFORMAT='%lU';time ( fib -i 113 ) 2>&1 )"
184551825793033096366333
0m0.005s
```

## Packages

Here I am using the following packages:

* [Control.Parallel](http://hackage.haskell.org/package/parallel) - to calculate sequence in parallel
* [System.Console.ParseArgs](http://hackage.haskell.org/package/parseargs) - to parse command line arguments:
* [Test.QuickCheck](http://hackage.haskell.org/package/QuickCheck) - unit testing

## Interactive

Invoke interactive Haskell session for Glasgow Haskell Compiler:

```bash
ghci app/Main.hs src/Fibonacci.hs
```

## Performance

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

### Example

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

