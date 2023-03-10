# Haskell

Simple project to calculate Fibonacci sequences.

API documentation available at:

* [GitHub](https://frankhjung.github.io/haskell-fibonacci/)
* [GitLab](https://frankhjung1.gitlab.io/haskell-fibonacci/)

## Build

This project can be built using either cabal or stack. Cabal seems to have an
advantage as it does not re-build dependencies when calling test, bench or
haddock.

Makefile targets are:

### check

This target will build tags using
[hasktags](https://hackage.haskell.org/package/hasktags), format code using
[stylish-haskell](http://hackage.haskell.org/package/stylish-haskell) and lint
using [hlint](http://hackage.haskell.org/package/hlint).

### build

Build library and command line application.

### test

Run unit tests.

### bench

Run performance benchmarks for each algorithm.

### doc

Build haddock documentation including source and quick-jump links.

### exec

Run application with example parameters.

```bash
$ stack exec fib -- -i 12
144
```

If installed, then the program `fib` will be on the path.

## Usage

```bash
$ fib -h
usage: fib [options]
  [-h,--help]               Help
  [-b,--binet <int>]        Generate Fibonacci using Binet formula
  [-f,--fast <int>]         Generate Fibonacci using fast algorithm
  [-i,--index <int>]        Generate Fibonacci at index
  [-p,--parallel <int>]     Generate Fibonacci in parallel
  [-s,--sequence <int>]     Generate Fibonacci sequence
  [-t,--traditional <int>]  Generate Fibonacci using traditional recursion
```

## Examples

Run fast, index and parallel for ``n = 44`` :

```bash
$ for o in '-b' '-f' '-i' '-p' '-t'; do \
    echo "fib ${o} 44 = $( TIMEFORMAT='%lU'; \
    time ( fib ${o} 44 +RTS -N2) 2>&1 )" ; \
  done

fib -b 44 = 701408733
0m0.001s
fib -f 44 = 701408733
0m0.001s
fib -i 44 = 701408733
0m0.005s
fib -p 44 = 701408733
0m31.910s
fib -t 44 = 701408733
0m20.171s
```

## Packages

Here I am using the following packages:

* [System.Console.ParseArgs](http://hackage.haskell.org/package/parseargs) - to parse command line arguments:
* [Test.QuickCheck](http://hackage.haskell.org/package/QuickCheck) - unit testing

## Interactive

Invoke interactive Haskell session for Glasgow Haskell Compiler:

```bash
ghci app/Main.hs src/Fibonacci.hs
```

### Example

```bash
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

## Measure Elapsed Time

Measure elapsed CPU user time with:

```bash
elapsed="$( TIMEFORMAT='%lU';time ( ./main -s 120 ) 2>&1 1>/dev/null )"
echo $elapsed
```

To profile program in interactive [GHCi](https://wiki.haskell.org/GHC/GHCi)
session:

```bash
# start session
$ ghci app/main.hs src/fib.hs

# now run commands
Prelude>  set +s
*Main>    :main -i 110
```

## Performance Measure using Fast

```bash
$ fib -f 44 +RTS -s
701408733
         120,560 bytes allocated in the heap
          15,792 bytes copied during GC
          56,816 bytes maximum residency (1 sample(s))
          29,200 bytes maximum slop
               2 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.002s  (  0.001s elapsed)
  MUT     time    0.000s  (  0.000s elapsed)
  GC      time    0.000s  (  0.000s elapsed)
  EXIT    time    0.000s  (  0.008s elapsed)
  Total   time    0.002s  (  0.010s elapsed)

  Alloc rate    449,257,324 bytes per MUT second

  Productivity  11.3% of total user, 2.7% of total elapsed
```

## Performance Measure using Index

```bash
$ fib -i 44 +RTS -s
701408733
         123,320 bytes allocated in the heap
          15,680 bytes copied during GC
          56,704 bytes maximum residency (1 sample(s))
          29,312 bytes maximum slop
               2 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    0.000s  (  0.000s elapsed)
  GC      time    0.000s  (  0.000s elapsed)
  EXIT    time    0.001s  (  0.009s elapsed)
  Total   time    0.002s  (  0.011s elapsed)

  Alloc rate    617,578,862 bytes per MUT second

  Productivity   9.0% of total user, 2.0% of total elapsed
```

## Performance Measure using Parallel

### 1 CPU

```bash
$ fib -p 44 +RTS -s -N1
701408733
  56,618,785,376 bytes allocated in the heap
      56,005,384 bytes copied during GC
          73,544 bytes maximum residency (4 sample(s))
          29,200 bytes maximum slop
               3 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     54101 colls,     0 par    0.359s   0.395s     0.0000s    0.0001s
  Gen  1         4 colls,     0 par    0.001s   0.001s     0.0002s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 1134903169 (0 converted, 691675009 overflowed, 0 dud, 8192 GC'd, 0 fizzled)

  INIT    time    0.002s  (  0.001s elapsed)
  MUT     time   29.088s  ( 29.044s elapsed)
  GC      time    0.359s  (  0.396s elapsed)
  EXIT    time    0.000s  (  0.010s elapsed)
  Total   time   29.449s  ( 29.451s elapsed)

  Alloc rate    1,946,477,099 bytes per MUT second

  Productivity  98.8% of total user, 98.6% of total elapsed
```

### 4 CPU's

```bash
$ fib -p 44 +RTS -s -N4
701408733
  57,204,209,624 bytes allocated in the heap
      56,587,328 bytes copied during GC
         439,200 bytes maximum residency (35 sample(s))
          57,440 bytes maximum slop
               6 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     15709 colls, 15709 par    4.605s   0.538s     0.0000s    0.0138s
  Gen  1        35 colls,    34 par    0.036s   0.005s     0.0001s    0.0003s

  Parallel GC work balance: 44.44% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 1146135554 (7099 converted, 668828118 overflowed, 0 dud, 5852028 GC'd, 48028405 fizzled)

  INIT    time    0.003s  (  0.001s elapsed)
  MUT     time   54.027s  ( 15.161s elapsed)
  GC      time    4.641s  (  0.543s elapsed)
  EXIT    time    0.001s  (  0.005s elapsed)
  Total   time   58.671s  ( 15.710s elapsed)

  Alloc rate    1,058,815,115 bytes per MUT second

  Productivity  92.1% of total user, 96.5% of total elapsed
```
