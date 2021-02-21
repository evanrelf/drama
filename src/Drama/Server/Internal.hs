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

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Drama.Process
import Drama.Process.Internal (Address (..), Message)

import qualified Control.Concurrent.Chan.Unagi as Unagi

-- Support `MonadFail` on GHC 8.6.5
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif
#if MIN_VERSION_base(4,13,0)
import Prelude hiding (MonadFail)
#endif


-- | TODO
--
-- @since 1.0.0.0
newtype Server msg a = Server { serverToProcess :: Process (Envelope msg) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , Alternative
#if MIN_VERSION_base(4,9,0)
    , MonadPlus
#endif
    , MonadFail
    , MonadFix
    )


-- | TODO
--
-- @since 1.0.0.0
data Envelope msg
  = Cast (msg ())
  | forall res. Message res => Call (Address res) (msg res)


-- | TODO
--
-- @since 1.0.0.0
start :: Server childMsg () -> Server msg (Address (Envelope childMsg))
start (Server process) = Server $ spawn process


-- | TODO
--
-- @since 1.0.0.0
cast :: Address (Envelope recipientMsg) -> recipientMsg () -> Server msg ()
cast address message = Server do
  send address (Cast message)


-- | TODO
--
-- @since 1.0.0.0
call :: Message res => Address (Envelope recipientMsg) -> recipientMsg res -> Server msg res
call address message = Server do
  (inChan, outChan) <- liftIO Unagi.newChan
  let returnAddress = Address inChan
  send address (Call returnAddress message)
  liftIO $ Unagi.readChan outChan


-- | TODO
--
-- @since 1.0.0.0
handle :: (forall res. msg res -> Server msg res) -> Server msg ()
handle callback = do
  envelope <- Server receive
  case envelope of
    Cast msg ->
      callback msg
    Call returnAddress msg -> do
      res <- callback msg
      Server $ send returnAddress res
