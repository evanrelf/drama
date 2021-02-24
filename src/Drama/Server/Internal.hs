{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module:     Drama.Server.Internal
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com

module Drama.Server.Internal where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind (Type)
import Drama.Process (HasMsg, Process, send)
import Drama.Process.Internal (Address (..))

import qualified Control.Concurrent.Chan.Unagi as Unagi


-- | @since 0.3.0.0
type Server msg a = Process (Envelope msg) a


-- | Wrapper around higher-kinded message types, to make them compatible with
-- the lower-level `Process` machinery.
--
-- Higher-kinded message types are defined as GADTs with a type parameter. This
-- allows specifying the response type for messages.
--
-- ===== __Example__
--
-- A server which encapsulates a piece of mutable state. Its @StateMsg@ type
-- specifies which messages it accepts, which messages return a response, and
-- what type that response is.
--
-- > data StateMsg s res where
-- >   GetState :: StateMsg s s
-- >   GetsState :: (s -> a) -> StateMsg s a
-- >   PutState :: s -> StateMsg s ()
-- >   ModifyState :: (s -> s) -> StateMsg s ()
-- >
-- > state :: s -> Server (StateMsg s) ()
-- > state s0 = do
-- >   stateIORef <- liftIO $ newIORef s0
-- >
-- >   forever $ receive >>= handle \case
-- >     GetState ->
-- >       liftIO $ readIORef stateIORef
-- >
-- >     GetsState f -> do
-- >       s <- liftIO $ readIORef stateIORef
-- >       pure (f s)
-- >
-- >     PutState s ->
-- >       liftIO $ writeIORef stateIORef s
-- >
-- >     ModifyState f ->
-- >       liftIO $ modifyIORef stateIORef f
--
-- @since 0.3.0.0
data Envelope (msg :: Type -> Type)
  = Cast !(msg ())
  -- ^ Case where message needs to response (produced by `cast`)
  | forall res. HasMsg res => Call !(Address res) !(msg res)
  -- ^ Case where message requires a response (produced by `call`)


-- | Send a message to another process, expecting no response. Returns
-- immediately without blocking.
--
-- @since 0.3.0.0
cast
  :: Address (Envelope msg)
  -- ^ Process' address
  -> msg ()
  -- ^ Message to send (has no response)
  -> Process _msg ()
cast addr msg = send addr (Cast msg)


-- | Send a message to another process, and wait for a response.
--
-- @since 0.3.0.0
call
  :: HasMsg res
  => Address (Envelope msg)
  -- ^ Process' address
  -> msg res
  -- ^ Message to send
  -> Process _msg res
  -- ^ Response
call addr msg = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let returnAddr = Address inChan
  send addr (Call returnAddr msg)
  liftIO $ Unagi.readChan outChan


-- | Handle messages which may require a response.
--
-- @since 0.3.0.0
handle
  :: (forall res. msg res -> Process _msg res)
  -- ^ Callback function that responds to messages
  -> Envelope msg
  -- ^ Message which may require a response
  -> Process _msg ()
handle callback = \case
  Cast msg ->
    callback msg
  Call returnAddr msg -> do
    res <- callback msg
    send returnAddr res
