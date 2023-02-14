{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import           Control.Exception (evaluate)
import           Fibonacci         (fibf, fibi, fibp, fibt)
import           Test.Hspec        (describe, errorCall, hspec, it, shouldThrow)
import           Test.QuickCheck

newtype Sample = Sample Int deriving stock Show

instance Arbitrary Sample where
  arbitrary = Sample . (`mod` 35) <$> arbitrary

-- Negative now part of QuickCheck Modifiers.
-- newtype Negative = Negative Int deriving Show

-- instance Arbitrary Main.Negative where
--   arbitrary = (Main.Negative . negate) . getPositive <$> arbitrary

-- 'fibi' is the reference implementation. (It is also the fastest
-- algorithm).
main :: IO ()
main = hspec $ do

  describe "fibf" $
    it "fibi == fibf" $
      property $ \(Sample n) -> fibi n == fibf n

  describe "fibf < 0" $
    it "fibf error for negatives" $
      property $ \(Negative n) -> evaluate (fibf n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

  describe "fibi < 0" $
    it "fibi error for negatives" $
      property $ \(Negative n) -> evaluate (fibi n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

  describe "fibp" $
    it "fibi == fibp" $
      property $ \(Sample n) -> fibi n == fibp n

  describe "fibp < 0" $
    it "fibp error for negatives" $
      property $ \(Negative n) -> evaluate (fibp n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"

  describe "fibt" $
    it "fibi == fibt" $
      property $ \(Sample n) -> fibi n == fibt n

  describe "fibt < 0" $
    it "fibt error for negatives" $
      property $ \(Negative n) -> evaluate (fibt n) `shouldThrow` errorCall "fibonacci only defined on natural numbers"
