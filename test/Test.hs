module Main (main) where

import           Control.Exception         (evaluate)
import           Fibonacci                 (fibf, fibi, fibp)
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

main :: IO ()
main = hspec $ do

  describe "fibf" $
    it "fibi == fibf" $
      property $ \(Sample n) -> fibi n == fibf n

  describe "fibi" $
    it "fibi == fibp" $
      property $ \(Sample n) -> fibi n == fibp n

  describe "fibf < 0" $
    it "fibf error for negatives" $
      property $ \(Negative n) -> evaluate (fibf n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

  describe "fibi < 0" $
    it "fibi error for negatives" $
      property $ \(Negative n) -> evaluate (fibi n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

  describe "fibp < 0" $
    it "fibp error for negatives" $
      property $ \(Negative n) -> evaluate (fibp n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

