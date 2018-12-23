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
fibi n
  | n == 0 = 1
  | n > 0  = last (take n fibs)
  | otherwise = error "fibonacci only defined on natural numbers"

-- | Generate a Fibonacci sequence.
fibs :: [Integer]
tfibs :: [Integer]
fibs@(1:tfibs) = 1 : 1 : [ a + b | (a, b) <- zip fibs tfibs ]

