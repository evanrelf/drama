{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module Starring.Demo where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Starring
import Prelude hiding (log)


main2 :: IO ()
main2 = run do
  loggerAddress <- spawn logger
  _ <- spawn (fizzBuzz loggerAddress)
  wait


logger :: Actor String ()
logger = forever do
  string <- receive
  liftIO $ putStrLn string


fizzBuzz :: Address String -> Actor () ()
fizzBuzz loggerAddress = do
  let log = send loggerAddress

  loop (1 :: Int) \n -> do
    if | n `mod` 15 == 0 -> log "FizzBuzz"
       | n `mod`  3 == 0 -> log "Fizz"
       | n `mod`  5 == 0 -> log "Buzz"
       | otherwise       -> log (show n)

    liftIO $ threadDelay 500_000

    continue (n + 1)


counter :: Actor msg Int
counter = loop 10 \count -> do
  liftIO $ print count
  if count > 0
    then continue (count - 1)
    else exit count


echo :: (forall msg. String -> Actor msg ()) -> Actor () ()
echo log = forever do
  line <- liftIO getLine
  if line == "ping" then
    log "pong"
  else
    log line


add1 :: (forall msg. String -> Actor msg ()) -> Actor (Address Int, Int) ()
add1 log = forever do
  log "[add1] Waiting for request"
  (returnAddress, number) <- receive
  log "[add1] Received request"
  log "[add1] Spawning worker"
  spawn (add1Worker log number returnAddress)


add1Worker :: (forall msg. String -> Actor msg ()) -> Int -> Address Int -> Actor () ()
add1Worker log number returnAddress = do
  log "[add1Worker] Started"
  send returnAddress (number + 1)
  log "[add1Worker] Replied"


program :: Actor Int ()
program = do
  liftIO $ putStrLn "[main] START"

  liftIO $ putStrLn "[main] Spawning logger"
  loggerAddress <- spawn logger

  let log = send loggerAddress

  liftIO $ putStrLn "[main] Spawning echo"
  _ <- spawn (echo log)

  log "[main] Spawning add1"
  add1Address <- spawn (add1 log)

  log "[main] Sending number to add1"
  address <- here
  send add1Address (address, 1)
  two <- receive
  log ("[main] Received response from add1: " <> show two)

  log "[main] FINISH"
  wait


main :: IO ()
main = run program
