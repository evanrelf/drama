{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Actress
  ( -- * Types
    Actor
  , Address
  , Mailbox
  , Scope

    -- * Functions
  , receive
  , send
  , spawn
  , wait
  , run
  , loop
  )
where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Ki


type Actor message = Address message -> Mailbox message -> Scope -> IO ()


newtype Address message = Address (Unagi.InChan message)


newtype Mailbox message = Mailbox (Unagi.OutChan message)


newtype Scope = Scope (Ki.Scope)


receive :: Mailbox message -> IO message
receive (Mailbox outChan) = Unagi.readChan outChan


send :: Address message -> message -> IO ()
send (Address inChan) message = Unagi.writeChan inChan message


spawn :: Scope -> Actor message -> IO (Address message)
spawn (Scope kiScope) actor = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.fork_ kiScope (Ki.scoped \childKiScope -> actor address mailbox (Scope childKiScope))
  pure address


wait :: Scope -> IO ()
wait (Scope kiScope) = Ki.wait kiScope


run :: Actor message -> IO ()
run actor = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.scoped \kiScope -> actor address mailbox (Scope kiScope)


loop :: a -> (a -> IO (Maybe a)) -> IO ()
loop x0 k = do
  k x0 >>= \case
    Just x -> loop x k
    Nothing -> pure ()
