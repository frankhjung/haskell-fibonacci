{-|
  Module      : Fibonacci
  Description : Demonstrate different Fibonacci number generators.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
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
  | n >= 0    = (fst . fibPair) n
  | otherwise = error "fibonacci only defined on natural numbers"

-- | Fibonacci Pairs.
fibPair :: Int -> (Integer, Integer)
fibPair n
  | n == 0    = (0,1)
  | otherwise = fibStep (fibPair (n - 1))

-- | Fibonacci Step.
fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

-- | Calculate the nth Fibonacci value.
--
-- This retreives value from head of sequence produced by 'fibs'.
fibi :: Int -> Integer
fibi n
  | n > 0     = last (take n fibs)
  | n == 0    = 0
  | otherwise = error "fibonacci only defined on natural numbers"

-- | Calculate Fibonacci in parallel.
--
-- From: "Practical Concurrent Haskell: With Big Data Applications",
-- Chapter 3, Parallelism and Concurrency with Haskell
fibp :: Int -> Integer
fibp n
  | n >  1    = x `par` (y `pseq` x + y)
  | n == 1    = 1
  | n == 0    = 0
  | otherwise = error "fibonacci only defined on natural numbers"
  where x = fibp (n - 1)
        y = fibp (n - 2)

-- | Generate a Fibonacci sequence.
--
-- Note: Sequence starts from 1 not 0.
fibs :: [Integer]
tfibs :: [Integer]
fibs@(1:tfibs) = 1 : 1 : [ a + b | (a, b) <- zip fibs tfibs ]
