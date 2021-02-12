{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Actress.Demo where

import Actress
import Control.Monad (forever)
import Prelude hiding (log)


logger :: Actor String
logger = new \Self{mailbox} -> loop (1 :: Int) \count -> do
  string <- receive mailbox
  putStrLn (show count <> ": " <> string)
  pure $ Just (count + 1)


echo :: (String -> IO ()) -> Actor ()
echo log = new \_ -> forever do
  line <- getLine
  if line == "ping" then
    log "pong"
  else
    log line


add1 :: (String -> IO ()) -> Actor (Address Int, Int)
add1 log = new \Self{mailbox, scope} -> forever do
  log "[add1] Waiting for request"
  (returnAddress, number) <- receive mailbox
  log "[add1] Received request"
  log "[add1] Spawning worker"
  spawn scope (add1Worker log number returnAddress)


add1Worker :: (String -> IO ()) -> Int -> Address Int -> Actor ()
add1Worker log number returnAddress = new \_ -> do
  log "[add1Worker] Started"
  send returnAddress (number + 1)
  log "[add1Worker] Replied"


program :: Actor Int
program = new \Self{address, mailbox, scope} -> do
  putStrLn "[main] START"

  putStrLn "[main] Spawning logger"
  loggerAddress <- spawn scope logger

  let log = send loggerAddress

  putStrLn "[main] Spawning echo"
  _ <- spawn scope (echo log)

  log "[main] Spawning add1"
  add1Address <- spawn scope (add1 log)

  log "[main] Sending number to add1"
  send add1Address (address, 1)
  two <- receive mailbox
  log ("[main] Received response from add1: " <> show two)

  log "[main] FINISH"
  wait scope


main :: IO ()
main = run program
