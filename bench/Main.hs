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
  summerAddr <- spawn (summer max)
  _ <- spawn (counter max summerAddr)
  wait


summer :: Int -> Process Int ()
summer max = loop (max, 0) \(count, sum) -> do
  n <- receive
  if count > 0
    then continue (count - 1, sum + n)
    else exit ()


counter :: Int -> Address Int -> Process msg ()
counter max summerAddr = loop 0 \count -> do
  send summerAddr count
  if count < max
    then continue (count + 1)
    else exit ()
