module Main (main) where

import           Control.Exception         (evaluate)
import           Fibonacci                 (fibi, fibp)
import           Test.Hspec                (describe, errorCall, hspec, it,
                                            shouldReturn, shouldThrow)
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

newtype Sample = Sample Int deriving Show

instance Arbitrary Sample where
  arbitrary = Sample . (`mod` 35) <$> arbitrary

newtype Negative = Negative Int deriving Show

instance Arbitrary Negative where
  arbitrary = (Negative . negate) . getPositive <$> arbitrary

-- | Fibonacci only valid for natural numbers.
-- So test for error on negative numbers
-- prop_error :: Int -> Property
-- prop_error n
--   | n < 0     = expectFailure $ fibi n
--   | otherwise = expectFailure $ fibi ((-1) - n)

main :: IO ()
main = hspec $ do

  describe "fibi" $
    it "fibi == fibp" $
      property $ \(Sample n) -> fibi n == fibp n

  describe "fibi < 0" $
    it "fibi error for negatives" $
      property $ \(Negative n) -> evaluate (fibi n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

  describe "fibp < 0" $
    it "fibp error for negatives" $
      property $ \(Negative n) -> evaluate (fibp n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

