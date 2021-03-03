{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
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

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind (Type)
import Drama.Process (Address, HasMsg, Process, send)


-- | @since 0.3.0.0
type Server msg a = Process (Envelope msg) a


-- | Wrapper around higher-kinded message types, to make them compatible with
-- the lower-level `Process` machinery.
--
-- Higher-kinded message types are defined as GADTs with a type parameter. This
-- allows specifying the response type for messages.
--
-- @since 0.3.0.0
data Envelope (msg :: Type -> Type) where
  Cast :: msg () -> Envelope msg
  Call :: HasMsg res => MVar res -> msg res -> Envelope msg


-- | Send a message to another process, expecting no response. Returns
-- immediately without blocking.
--
-- @since 0.3.0.0
cast
  :: Address (Envelope msg)
  -- ^ Process' address
  -> msg ()
  -- ^ Message to send
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
  resMVar <- liftIO newEmptyMVar
  send addr (Call resMVar msg)
  liftIO $ takeMVar resMVar


-- | Handle messages which may require a response. This is the only way to
-- consume an `Envelope`.
--
-- @since 0.3.0.0
handle
  :: (forall res. msg res -> Process _msg res)
  -- ^ Callback function that responds to messages
  -> Envelope msg
  -- ^ Message to handle
  -> Process _msg ()
handle callback = \case
  Cast msg ->
    callback msg
  Call resMVar msg -> do
    res <- callback msg
    liftIO $ putMVar resMVar res
