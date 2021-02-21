{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}


module Main (main) where

import Criterion.Types
import Criterion.Main
import Drama
import Prelude hiding (log, max, sum)


main :: IO ()
main = defaultMainWith defaultConfig{timeLimit = 30, verbosity = Verbose}
  [ bgroup "benchCounterSummer"
      [ bench "1_000_000" $ nfIO (benchCounterSummer 1_000_000)
      ]
  ]


benchCounterSummer :: Int -> IO ()
benchCounterSummer max = run do
  summerAddress <- spawn (summer max)
  _ <- spawn (counter max summerAddress)
  wait


summer :: Int -> Process Int ()
summer max = loop (max, 0) \(count, sum) -> do
  n <- receive
  if count > 0
    then continue (count - 1, sum + n)
    else exit ()


counter :: Int -> Address Int -> Process msg ()
counter max summerAddress = loop 0 \count -> do
  send summerAddress count
  if count < max
    then continue (count + 1)
    else exit ()
