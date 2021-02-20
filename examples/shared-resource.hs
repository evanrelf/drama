{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Drama
import Prelude hiding (log)


main :: IO ()
main = run do
  -- Spawn `logger` actor, which starts waiting for requests.
  loggerAddress <- spawn logger

  -- Spawn two noisy actors, `fizzBuzz` and `navi`, which want to constantly
  -- print to the console. Instead of running `putStrLn`, they will send
  -- messages to `logger`.
  _ <- spawn (fizzBuzz loggerAddress)
  _ <- spawn (navi loggerAddress)

  -- Block `main` thread forever.
  wait


-- | Actor which encapsulates access to the console (a shared resource). By
-- sending log messages to `logger`, instead of running `putStrLn` directly, we
-- can avoid interleaving logs from actors running in parallel.
logger :: Actor String ()
logger = forever do
  string <- receive
  liftIO $ putStrLn string


-- | Silly example actor which wants to print to the console
fizzBuzz :: Address String -> Actor () ()
fizzBuzz loggerAddress = do
  let log = send loggerAddress

  loop (0 :: Int) \n -> do
    if | n `mod` 15 == 0 -> log "FizzBuzz"
       | n `mod`  3 == 0 -> log "Fizz"
       | n `mod`  5 == 0 -> log "Buzz"
       | otherwise       -> log (show n)

    -- Delay included for nicer (slow) output
    liftIO $ threadDelay 500_000

    continue (n + 1)


-- | Silly example actor which wants to print to the console
navi :: Address String -> Actor () ()
navi loggerAddress = do
  let log = send loggerAddress

  forever do
    log "Hey, listen!"

    -- Delay included for nicer (slow) output
    liftIO $ threadDelay 1_000_000
