{-# LANGUAGE UnicodeSyntax #-}

{-| Module      : Main
    Description : Benchmark suite for Fibonacci algorithms
    Copyright   : © Frank Jung, 2018-2025
    License     : GPL-3
    Maintainer  : frankhjung@linux.com
    Stability   : stable
    Portability : portable
-}

module Main (main) where

import           Criterion.Main (Benchmark (..), bench, bgroup, defaultMain,
                                 whnf)
import           Fibonacci      (fibb, fibi, fibp, fibr, fibt)

-- | Benchmark Fibonacci algorithms.
main :: IO ()
main = defaultMain [
  bgroup "fibb"
    [ bench  "5" $ whnf fibb  5
    , bench "34" $ whnf fibb 34
    ],
  bgroup "fibi"
    [ bench  "5" $ whnf fibi  5
    , bench "34" $ whnf fibi 34
    ],
  bgroup "fibp"
    [ bench  "5" $ whnf fibp  5
    , bench "34" $ whnf fibp 34
    ],
  bgroup "fibr"
    [ bench  "5" $ whnf fibr  5
    , bench "34" $ whnf fibr 34
    ],
  bgroup "fibt"
    [ bench  "5" $ whnf fibt  5
    , bench "34" $ whnf fibt 34
    ]
  ]
