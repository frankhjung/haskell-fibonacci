# Haskell

Simple project to calculate Fibonacci sequences.

## Usage

```bash
usage: fib [options]
  [-h,--help]            Help
  [-f,--fast <int>]      Generate Fibonacci using fast(er) algorithm
  [-i,--index <int>]     Generate Fibonacci at index
  [-p,--parallel <int>]  Generate Fibonacci in parallel
  [-s,--sequence <int>]  Generate Fibonacci sequence
```

## Example

```bash
echo "$( TIMEFORMAT='%lU';time ( fib -i 113 ) 2>&1 )"
184551825793033096366333
0m0.005s
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

## Measure Elapsed Time

Measure elapsed CPU user time with:

```bash
elapsed="$( TIMEFORMAT='%lU';time ( ./main -s 120 ) 2>&1 1>/dev/null )"
echo $elapsed
```

To profile program in interactive [GHC](https://wiki.haskell.org/GHC/GHCi)
session:

```c2hs
# start session
$ ghci app/main.hs src/fib.hs

# now run commands
Prelude>  set +s
*Main>    :main -i 110
```

## Performance Measure using Index

```bash
$ fib -i 40 +RTS -s
102334155
         121,312 bytes allocated in the heap
          15,800 bytes copied during GC
          56,864 bytes maximum residency (1 sample(s))
          29,152 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    0.001s  (  0.001s elapsed)
  GC      time    0.000s  (  0.000s elapsed)
  EXIT    time    0.001s  (  0.009s elapsed)
  Total   time    0.004s  (  0.011s elapsed)

  Alloc rate    103,128,663 bytes per MUT second

  Productivity  68.0% of total user, 89.4% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
```

## Performance Measure using Parallel

### 1 CPU

```bash
$ fib -p 40 +RTS -s 
102334155
   5,611,387,864 bytes allocated in the heap
       5,619,896 bytes copied during GC
          73,408 bytes maximum residency (2 sample(s))
          29,272 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      5361 colls,     0 par    0.185s   0.185s     0.0000s    0.0002s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 165580140 (0 converted, 121660603 overflowed, 0 dud, 43916911 GC'd, 2626 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    3.856s  (  3.854s elapsed)
  GC      time    0.185s  (  0.185s elapsed)
  EXIT    time    0.001s  (  0.010s elapsed)
  Total   time    4.043s  (  4.051s elapsed)

  Alloc rate    1,455,163,270 bytes per MUT second

  Productivity  95.4% of total user, 95.4% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
```

### 4 CPU's

```bash
$ fib -p 40 +RTS -s -N4
102334155
   5,618,333,144 bytes allocated in the heap
       4,980,088 bytes copied during GC
         207,304 bytes maximum residency (2 sample(s))
          53,272 bytes maximum slop
               6 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1352 colls,  1352 par    4.628s   0.186s     0.0001s    0.0015s
  Gen  1         2 colls,     1 par    0.005s   0.000s     0.0001s    0.0002s

  Parallel GC work balance: 166.41% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 165768164 (88 converted, 121328717 overflowed, 0 dud, 43995766 GC'd, 443593 fizzled)

  INIT    time    0.004s  (  0.002s elapsed)
  MUT     time    2.605s  (  1.638s elapsed)
  GC      time    4.633s  (  0.186s elapsed)
  EXIT    time    0.002s  (  0.005s elapsed)
  Total   time    7.244s  (  1.832s elapsed)

  Alloc rate    2,157,040,729 bytes per MUT second

  Productivity  36.0% of total user, 89.7% of total elapsed

gc_alloc_block_sync: 1533
whitehole_spin: 0
gen[0].sync: 13
gen[1].sync: 10
```

