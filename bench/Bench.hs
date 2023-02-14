{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import           Criterion.Main (Benchmark (..), bench, bgroup, defaultMain,
                                 whnf)
import           Fibonacci      (fibf, fibi, fibp, fibt)

-- | Benchmark Fibonacci algorithms.
main :: IO ()
main = defaultMain [
  bgroup "fibf"
    [ bench  "5" $ whnf fibf  5
    , bench "34" $ whnf fibf 34
    ],
  bgroup "fibi"
    [ bench  "5" $ whnf fibi  5
    , bench "34" $ whnf fibi 34
    ],
  bgroup "fibp"
    [ bench  "5" $ whnf fibi  5
    , bench "34" $ whnf fibi 34
    ],
  bgroup "fibt"
    [ bench  "5" $ whnf fibi  5
    , bench "34" $ whnf fibi 34
    ]
  ]
