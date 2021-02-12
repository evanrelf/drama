{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}


module Actress
  ( -- * Types
    Actor
  , Address
  , Mailbox

    -- * Functions
  , receive
  , send
  , spawn
  , run
  , loop

    -- * Re-exports
  , Ki.Scope
  , Ki.wait
  )
where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Ki


type Actor message = Address message -> Mailbox message -> Ki.Scope -> IO ()


newtype Address message = Address (Unagi.InChan message)


newtype Mailbox message = Mailbox (Unagi.OutChan message)


receive :: Mailbox message -> IO message
receive (Mailbox outChan) = Unagi.readChan outChan


send :: Address message -> message -> IO ()
send (Address inChan) message = Unagi.writeChan inChan message


spawn :: Ki.Scope -> Actor message -> IO (Address message)
spawn scope actor = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.fork_ scope (Ki.scoped \childScope -> actor address mailbox childScope)
  pure address


run :: Actor message -> IO ()
run actor = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.scoped \childScope -> actor address mailbox childScope


loop :: a -> (a -> IO (Maybe a)) -> IO ()
loop x0 k = do
  k x0 >>= \case
    Just x -> loop x k
    Nothing -> pure ()
