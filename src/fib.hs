module Fibonacci where

--
-- fibonacci sequence
--
fibs :: [Integer]
tfibs :: [Integer]
fibs@(1:tfibs) = 1 : 1 : [ a+b | (a,b) <- zip fibs tfibs ]

-- above should be equivalent to
-- fibs = 1 : 1 : [ a+b | (a,b) <- zip fibs (tail fibs) ]

-- fibonacci value at index
fibi :: Int -> Integer
fibi n = fibs !! n
-- fibi n = last (take n fibs)

--
-- WARNING: this method is way too slow
--
-- fibi :: Int -> Integer
-- fibi n
--     | n < 2     = 1
--     | otherwise = fibi(n-1) + fibi(n-2)
