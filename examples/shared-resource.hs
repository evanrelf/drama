{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Drama
import Prelude hiding (log)


main :: IO ()
main = runActor_ do
  -- Spawn `logger` actor, which starts waiting for requests.
  loggerAddr <- spawn logger

  -- Spawn two noisy actors, `fizzBuzz` and `navi`, which want to constantly
  -- print to the console. Instead of running `putStrLn`, they will send
  -- messages to `logger`.
  spawn_ (fizzBuzz loggerAddr)
  spawn_ (navi loggerAddr)

  -- Block `main` thread forever.
  wait


-- | Message type for `logger`
data LogMsg res where
  LogMsg :: String -> LogMsg ()


-- | Actor which encapsulates access to the console (a shared resource). By
-- sending log messages to `logger`, instead of running `putStrLn` directly, we
-- can avoid interleaving logs from actors running in parallel.
logger :: Actor LogMsg ()
logger = forever $ receive \case
  LogMsg string -> liftIO $ putStrLn string


-- | Silly example actor which wants to print to the console
fizzBuzz :: Address LogMsg -> Actor_ ()
fizzBuzz loggerAddr = go 0
  where
    log :: String -> Actor_ ()
    log string = cast loggerAddr (LogMsg string)

    go :: Int -> Actor_ ()
    go n = do
      if | n `mod` 15 == 0 -> log "FizzBuzz"
         | n `mod`  3 == 0 -> log "Fizz"
         | n `mod`  5 == 0 -> log "Buzz"
         | otherwise       -> log (show n)

      -- Delay included for nicer (slow) output
      liftIO $ threadDelay 500_000

      go (n + 1)


-- | Silly example actor which wants to print to the console
navi :: Address LogMsg -> Actor_ ()
navi loggerAddr = do
  let log string = cast loggerAddr (LogMsg string)

  forever do
    log "Hey, listen!"

    -- Delay included for nicer (slow) output
    liftIO $ threadDelay 1_000_000
