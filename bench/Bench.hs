{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import           Criterion.Main (Benchmark (..), bench, bgroup, defaultMain,
                                 whnf)
import           Fibonacci      (fibf, fibi)

-- | Benchmark Fibonacci algorithms.
main :: IO ()
main = defaultMain [
  bgroup "fibf"
    [ bench  "5" $ whnf fibf  5
    , bench "12" $ whnf fibf 12
    , bench "23" $ whnf fibf 23
    , bench "34" $ whnf fibf 34
    ],
  bgroup "fibi"
    [ bench  "5" $ whnf fibi  5
    , bench "12" $ whnf fibi 12
    , bench "23" $ whnf fibi 23
    , bench "34" $ whnf fibi 34
    ]
  ]
