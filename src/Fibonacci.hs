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
                 , fibi
                 , fibp
                 , fibr
                 , fibs
                 , fibt
                 ) where

import           Control.Parallel (par, pseq)

-- | Binet Fibonacci.
--
-- Uses the <https://doi.org/10.1016/j.ejc.2007.03.004 Binet Formula> to
-- generate the nth Fibonacci number.
fibb :: Int -> Integer
fibb n
  | n < 0     = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = round $ (phi^n - (1-phi)^n) / sqrt 5
  where
    phi = (1 + sqrt 5) / 2 :: Double

-- | Fibonacci Index.
--
-- This retrieves the nth Fibonacci value by index from a sequence
-- produced by 'fibs'.
fibi :: Int -> Integer
fibi n
  | n < 0 = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = fibs !! n

-- | Parallel Fibonacci.
--
-- Calculate Fibonacci in parallel.
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

-- | Fibonacci using code from Rebecca Skinner.
--
-- From: "Effective Haskell" by Rebecca Skinner
-- Chapter 2, Hands-On With Infinite Fibonacci Numbers
fibr :: Int -> Integer
fibr n
  | n < 0 = error "fibonacci only defined on natural numbers"
  | otherwise = fibr' !! n
  where
    fibr' :: [Integer]
    fibr' = 0 : 1 : go fibr' (tail fibr')
    go :: [Integer] -> [Integer] -> [Integer]
    go (x:xs) (y:ys) = (x + y) : go xs ys
    go _ _           = error "impossible case"

-- | Fibonacci Sequence.
--
-- The `fibs` function is a definition of an
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
-- This is very similar to the `fibr` function, but it uses `zipWith`.
--
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Traditional Fibonacci.
--
-- The traditional recursive Fibonacci.
fibt :: Int -> Integer
fibt n
  | n < 0     = error "fibonacci only defined on natural numbers"
  | n <= 1    = toInteger n
  | otherwise = fibt (n - 1) + fibt (n - 2)
