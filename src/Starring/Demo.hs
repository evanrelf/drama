{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE RankNTypes #-}

module Starring.Demo where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (forever)
import Prelude hiding (log)
import Starring


logger :: Actor String ()
logger = loop (1 :: Int) \count -> do
  string <- receive
  liftIO $ putStrLn (show count <> ": " <> string)
  continue (count + 1)


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
