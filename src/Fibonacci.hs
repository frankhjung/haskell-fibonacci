{-|
  Module      : Fibonacci
  Description : Demonstrate different Fibonacci number generators.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
-}

module Fibonacci ( fibi
                 , fibp
                 , fibs
                 ) where

import           Control.Parallel

-- | Calculate the nth Fibonacci value.
fibi :: Int -> Integer
fibi n
  | n <= 0    = 0
  | n <= 2    = 1
  | otherwise = fibs !! (n - 1)
-- fibi n = last (take n fibs)

-- | Calculate the nth Fibonacci value in parallel.
--
--   WARNING: this is slow
--
-- This is the fixed version from:
--    Source:  Practical Concurrent Haskell: With Big Data Applications
--    Chapter: (3) Parallelism and Concurrency with Haskell
--    By:      Stefania Loredana Nita,Marius Mihailescu, Apress © 2017
--    ISBN:    9781484227800
fibp :: Int -> Integer
fibp n
  | n <= 0    = 0
  | n <= 2    = 1
  | otherwise = par n1 (pseq n2 (n1 + n2))
                where n1 = fibp (n - 1)
                      n2 = fibp (n - 2)

-- | Generate a Fibonacci sequence.
fibs :: [Integer]
tfibs :: [Integer]
fibs@(1:tfibs) = 1 : 1 : [ a + b | (a, b) <- zip fibs tfibs ]

-- | WARNING: the following method is way too slow
--
-- fibi :: Int -> Integer
-- fibi n
--     | n <= 0    = 0
--     | n <= 2    = 1
--     | otherwise = fibi(n - 1) + fibi(n - 2)

