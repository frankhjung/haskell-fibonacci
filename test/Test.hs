module Main (main) where

import           Fibonacci       (fibi, fibp)
import           Test.QuickCheck

-- | fibi uses fibs under the hood, so just need to test fibi and fibp
prop_fib :: Int -> Bool
prop_fib n = fibi n == fibp n

-- Test small integer in range [-1, 30]
main :: IO ()
main = quickCheck $ forAll (choose (-1, 30)) prop_fib
