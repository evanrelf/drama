{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Drama
import Prelude hiding (log)


main :: IO ()
main = run_ do
  -- Spawn `logger` process, which starts waiting for requests.
  loggerAddr <- spawn logger

  -- Spawn two noisy processes, `fizzBuzz` and `navi`, which want to constantly
  -- print to the console. Instead of running `putStrLn`, they will send
  -- messages to `logger`.
  spawn_ (fizzBuzz loggerAddr)
  spawn_ (navi loggerAddr)

  -- Block `main` thread forever.
  wait


-- | Process which encapsulates access to the console (a shared resource). By
-- sending log messages to `logger`, instead of running `putStrLn` directly, we
-- can avoid interleaving logs from processes running in parallel.
logger :: Process String ()
logger = forever do
  string <- receive
  liftIO $ putStrLn string


-- | Silly example process which wants to print to the console
fizzBuzz :: Address String -> Process NoMsg ()
fizzBuzz loggerAddr = go 0
  where
    log :: String -> Process NoMsg ()
    log = send loggerAddr

    go :: Int -> Process NoMsg ()
    go n = do
      if | n `mod` 15 == 0 -> log "FizzBuzz"
         | n `mod`  3 == 0 -> log "Fizz"
         | n `mod`  5 == 0 -> log "Buzz"
         | otherwise       -> log (show n)

      -- Delay included for nicer (slow) output
      liftIO $ threadDelay 500_000

      go (n + 1)


-- | Silly example process which wants to print to the console
navi :: Address String -> Process NoMsg ()
navi loggerAddr = do
  let log = send loggerAddr

  forever do
    log "Hey, listen!"

    -- Delay included for nicer (slow) output
    liftIO $ threadDelay 1_000_000
