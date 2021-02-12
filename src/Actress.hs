{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Actress
  ( -- * Types
    Actor
  , Address
  , Mailbox
  , Scope

    -- * Functions
  , new
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


newtype Actor msg = Actor
  { _unActor
      :: Address msg
      -> Mailbox msg
      -> Scope
      -> IO ()
  }


newtype Address msg = Address (Unagi.InChan msg)


newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


newtype Scope = Scope (Ki.Scope)


new :: (Address msg -> Mailbox msg -> Scope -> IO ()) -> Actor msg
new = Actor


receive :: Mailbox msg -> IO msg
receive (Mailbox outChan) = Unagi.readChan outChan


send :: Address msg -> msg -> IO ()
send (Address inChan) msg = Unagi.writeChan inChan msg


spawn :: Scope -> Actor msg -> IO (Address msg)
spawn (Scope kiScope) (Actor actorFn) = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.fork_ kiScope (Ki.scoped \childKiScope -> actorFn address mailbox (Scope childKiScope))
  pure address


wait :: Scope -> IO ()
wait (Scope kiScope) = Ki.wait kiScope


run :: Actor msg -> IO ()
run (Actor actorFn) = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.scoped \kiScope -> actorFn address mailbox (Scope kiScope)


loop :: a -> (a -> IO (Maybe a)) -> IO ()
loop x0 k = do
  k x0 >>= \case
    Just x -> loop x k
    Nothing -> pure ()
