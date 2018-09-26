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
                 , fibs
                 ) where

-- | Calculate the nth Fibonacci value.
fibi :: Int -> Integer
fibi n = last (take n fibs)
--  | n <= 0    = 0
--  | n <= 2    = 1
--  | otherwise = fibs !! (n - 1)

-- | Generate a Fibonacci sequence.
fibs :: [Integer]
tfibs :: [Integer]
fibs@(1:tfibs) = 1 : 1 : [ a + b | (a, b) <- zip fibs tfibs ]

