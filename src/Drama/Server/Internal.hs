{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Drama.Process (Process, receive, send)
import Drama.Process.Internal (Address (..), Message)

import qualified Control.Concurrent.Chan.Unagi as Unagi

-- | TODO
--
-- @since 1.0.0.0
type Server msg a = Process (Envelope msg) a


-- | TODO
--
-- @since 1.0.0.0
data Envelope msg
  = Cast (msg ())
  | forall res. Message res => Call (Address res) (msg res)


-- | TODO
--
-- @since 1.0.0.0
cast :: Address (Envelope recipientMsg) -> recipientMsg () -> Process msg ()
cast address message = do
  send address (Cast message)


-- | TODO
--
-- @since 1.0.0.0
call :: Message res => Address (Envelope recipientMsg) -> recipientMsg res -> Process msg res
call address message = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let returnAddress = Address inChan
  send address (Call returnAddress message)
  liftIO $ Unagi.readChan outChan


-- | TODO
--
-- @since 1.0.0.0
handle :: (forall res. msg res -> Server msg res) -> Server msg ()
handle callback = do
  envelope <- receive
  case envelope of
    Cast msg ->
      callback msg
    Call returnAddress msg -> do
      res <- callback msg
      send returnAddress res
