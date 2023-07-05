{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import           Control.Exception (evaluate)
import           Fibonacci         (fibb, fibi, fibp, fibr, fibt)
import           Test.Hspec        (describe, errorCall, hspec, it, shouldThrow)
import           Test.QuickCheck

newtype Sample = Sample Int deriving stock Show

instance Arbitrary Sample where
  arbitrary = Sample . (`mod` 35) <$> arbitrary

-- Negative now part of QuickCheck Modifiers.
-- newtype Negative = Negative Int deriving Show

-- instance Arbitrary Main.Negative where
--   arbitrary = (Main.Negative . negate) . getPositive <$> arbitrary

-- Expected error message.
fiberr :: String
fiberr = "fibonacci only defined on natural numbers"

-- 'fibi' is the reference implementation. (It is also the fastest
-- algorithm).
main :: IO ()
main = hspec $ do

  describe "fibb" $
    it "fibb == fibi" $
      property $ \(Sample n) -> fibb n == fibi n

  describe "fibb < 0" $
    it "fibb error for negatives" $
      property $ \(Negative n) -> evaluate (fibb n) `shouldThrow` errorCall fiberr

  describe "fibi < 0" $
    it "fibi error for negatives" $
      property $ \(Negative n) -> evaluate (fibi n) `shouldThrow` errorCall fiberr

  describe "fibp" $
    it "fibp == fibi" $
      property $ \(Sample n) -> fibp n == fibi n

  describe "fibp < 0" $
    it "fibp error for negatives" $
      property $ \(Negative n) -> evaluate (fibp n) `shouldThrow` errorCall fiberr

  describe "fibr" $
    it "fibr == fibi" $
      property $ \(Sample n) -> fibr n == fibi n

  describe "fibr < 0" $
    it "fibr error for negatives" $
      property $ \(Negative n) -> evaluate (fibr n) `shouldThrow` errorCall fiberr

  describe "fibt" $
    it "fibt == fibi" $
      property $ \(Sample n) -> fibt n == fibi n

  describe "fibt < 0" $
    it "fibt error for negatives" $
      property $ \(Negative n) -> evaluate (fibt n) `shouldThrow` errorCall fiberr
