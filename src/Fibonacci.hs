{-|

  Module      : Fibonacci
  Description : Demonstrate different Fibonacci number generators.
  Copyright   : © Frank Jung, 2018-2021
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable

  == /TODO/

  Implement <https://doi.org/10.1016/j.ejc.2007.03.004 Binet Formula>
  for generating nth Fibonacci number:

@
  fn = (a^n-b^n) / sqrt(5)
  where
    a = (1+sqrt(5))/2
    b = (1-sqrt(5))/2
    n = Interger >= 1
@

-}

module Fibonacci ( fibf
                 , fibi
                 , fibp
                 , fibs
                 ) where

import           Control.Parallel (par, pseq)

-- | Fast Fibonacci.
--
-- From: "Haskell - the craft of functional programming"
-- Chapter 5, Data Types, Tuples and Lists
fibf :: Int -> Integer
fibf n
  | n < 0     = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = (fst . fibPair) n

-- | Fibonacci Pairs.
fibPair :: Int -> (Integer, Integer)
fibPair n
  | n == 0    = (0, 1)
  | otherwise = fibStep (fibPair (n - 1))

-- | Fibonacci Step.
fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

-- | Calculate the nth Fibonacci value.
--
-- This retreives value from index of sequence produced by 'fibs'.
fibi :: Int -> Integer
fibi n
  | n < 0 = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = fibs !! n

-- | Calculate Fibonacci in parallel.
--
-- From: "Practical Concurrent Haskell: With Big Data Applications",
-- Chapter 3, Parallelism and Concurrency with Haskell
fibp :: Int -> Integer
fibp n
  | n <  0    = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = x `par` (y `pseq` x + y)
  where x = fibp (n - 1)
        y = fibp (n - 2)

-- | Generate a Fibonacci sequence.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
