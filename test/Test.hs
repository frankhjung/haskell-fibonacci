module Main (main) where

import           Fibonacci       (fibi)
import           Test.QuickCheck

-- | First 10 Fibonacci numbers
-- fibi uses fibs under the hood
prop_fib :: Int -> Bool
prop_fib n = fibi n == fib10!!(n-1)
  where fib10 = [1,1,2,3,5,8,13,21,34,55]

-- | Fibonacci only valid for natural numbers
-- so test for error on negative numbers
-- prop_error :: Int -> Property
-- prop_error n
--   | n < 0     = expectFailure $ fibi n
--   | otherwise = expectFailure $ fibi ((-1) - n)

main :: IO ()
main = quickCheck $ forAll (elements [1..10]) prop_fib
-- quickCheck prop_error

