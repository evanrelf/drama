{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}

module SharedResource (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Drama
import Prelude hiding (log)


main :: IO ()
main = run do
  loggerAddress <- spawn logger

  _ <- spawn (fizzBuzz loggerAddress)
  _ <- spawn (navi loggerAddress)

  wait


logger :: Actor String ()
logger = forever do
  string <- receive
  liftIO $ putStrLn string


fizzBuzz :: Address String -> Actor () ()
fizzBuzz loggerAddress = do
  let log = send loggerAddress

  loop (0 :: Int) \n -> do
    if | n `mod` 15 == 0 -> log "FizzBuzz"
       | n `mod`  3 == 0 -> log "Fizz"
       | n `mod`  5 == 0 -> log "Buzz"
       | otherwise       -> log (show n)

    liftIO $ threadDelay 500_000

    continue (n + 1)


navi :: Address String -> Actor () ()
navi loggerAddress = do
  let log = send loggerAddress
  forever do
    log "Hey, listen!"
    liftIO $ threadDelay 1_000_000
