{-|

  Module      : Fibonacci
  Description : Demonstrate different Fibonacci number generators.
  Copyright   : © Frank Jung, 2018-2021
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable

-}

module Fibonacci ( fibb
                 , fibf
                 , fibi
                 , fibp
                 , fibs
                 , fibt
                 ) where

import           Control.Parallel (par, pseq)

-- | <https://doi.org/10.1016/j.ejc.2007.03.004 Binet Formula> for generating
-- nth Fibonacci number.
fibb :: Int -> Integer
fibb n
  | n < 0     = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = round $ (a^n - b^n) / sqrt 5
  where a = (1 + sqrt 5) / 2 :: Double
        b = (1 - sqrt 5) / 2 :: Double

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
-- This retrieves value from index of sequence produced by 'fibs'.
fibi :: Int -> Integer
fibi n
  | n < 0 = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = fibs !! n

-- | Generate a Fibonacci sequence. The `fibs` function is a definition of an
-- infinite list of Fibonacci numbers. It is defined recursively, by starting
-- with the two numbers 0 and 1, and then generating the rest of the sequence by
-- adding adjacent pairs of numbers in the list.
--
-- The `zipWith` function is used to create a list where each element is the sum
-- of the previous two elements in the list. Specifically, @zipWith (+) fibs
-- (tail fibs)@ takes two lists as input: `fibs`, which is the original sequence
-- starting with 0 and 1, and @tail fibs@, which is the same sequence but with
-- the first element removed. It applies the @+@ function to each corresponding
-- pair of elements in the two lists, resulting in a new list where each element
-- is the sum of the previous two elements in `fibs`.
--
-- The resulting list is then prepended with 0 and 1 to create the full
-- sequence, which is an infinite list of Fibonacci numbers [0, 1, 1, 2, 3, 5,
-- 8, 13, 21, ...].
--
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Calculate Fibonacci in parallel.
--
-- From: "Practical Concurrent Haskell: With Big Data Applications",
-- Chapter 3, Parallelism and Concurrency with Haskell
fibp :: Int -> Integer
fibp n
  | n <  0    = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = x `par` y `pseq` x + y
  where x = fibp (n - 1)
        y = fibp (n - 2)

-- | Traditional recursive Fibonacci.
fibt :: Int -> Integer
fibt n
  | n < 0     = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = fibt (n - 1) + fibt (n - 2)
