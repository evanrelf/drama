{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- For `HasMsg msg`
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module:     Drama.Process.Internal
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com

module Drama.Process.Internal where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Data.Kind (Constraint)
import Data.Void (Void)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Ki

-- Support `MonadFail` on GHC 8.6.5
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif
#if MIN_VERSION_base(4,13,0)
import Prelude hiding (MonadFail)
#endif


-- | TODO
--
-- @since 0.3.0.0
newtype Process msg a = Process (ReaderT (ProcessEnv msg) IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , Alternative
    , MonadPlus
#if MIN_VERSION_base(4,9,0)
    , MonadFail
#endif
    , MonadFix
    )


-- | TODO
--
-- @since 0.3.0.0
runProcess :: MonadIO m => ProcessEnv msg -> Process msg a -> m a
runProcess processEnv (Process reader) = liftIO $ runReaderT reader processEnv


-- | TODO
--
-- @since 0.3.0.0
data ProcessEnv msg = ProcessEnv
  { address :: Address msg
    -- ^ TODO
  , mailbox :: Mailbox msg
    -- ^ TODO
  , scope :: !Scope
    -- ^ TODO
  }


-- | TODO
--
-- @since 0.3.0.0
newtype Address msg = Address (Unagi.InChan msg)


-- | TODO
--
-- @since 0.3.0.0
newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


-- | TODO
--
-- @since 0.3.0.0
newtype Scope = Scope Ki.Scope


-- | TODO
--
-- @since 0.3.0.0
type family HasMsg msg :: Constraint where
  HasMsg NoMsg = TypeError ('Text "Processes with 'msg ~ NoMsg' cannot receive messages")
  HasMsg Void = TypeError ('Text "Use 'msg ~ NoMsg' instead of 'msg ~ Void' for processes which do not receive messages")
  HasMsg () = TypeError ('Text "Use 'msg ~ NoMsg' instead of 'msg ~ ()' for processes which do not receive messages")
  HasMsg msg = ()


-- | TODO
--
-- @since 0.3.0.0
data NoMsg


-- | TODO
--
-- @since 0.3.0.0
spawn
  :: HasMsg msg
  => Process msg ()
  -- ^ Process to spawn
  -> Process _msg (Address msg)
  -- ^ Spawned process' address
spawn process = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  spawnImpl address mailbox process
  pure address


-- | More efficient version of `spawn`, for processes which receive no messages
-- (@msg ~ `NoMsg`@). See docs for `spawn` for more information.
--
-- @since 0.3.0.0
spawn_ :: Process NoMsg () -> Process msg ()
spawn_ process = do
  let address = Address (error voidMsgError)
  let mailbox = Mailbox (error voidMsgError)
  spawnImpl address mailbox process


spawnImpl
  :: Address msg
  -> Mailbox msg
  -> Process msg ()
  -> Process _msg ()
spawnImpl address mailbox process = do
  Scope kiScope <- Process $ asks scope
  liftIO $ Ki.fork_ kiScope $ runImpl address mailbox process


-- | Block until all child processes have terminated.
--
-- @since 0.3.0.0
wait :: Process msg ()
wait = do
  Scope kiScope <- Process $ asks scope
  liftIO $ Ki.wait kiScope


-- | Return the current process' address.
--
-- @since 0.3.0.0
here :: HasMsg msg => Process msg (Address msg)
here = Process $ asks address


-- | Send a message to another process.
--
-- @since 0.3.0.0
send
  :: HasMsg msg
  => Address msg
  -- ^ Other process' address
  -> msg
  -- ^ Message to send
  -> Process _msg ()
send (Address inChan) msg = liftIO $ Unagi.writeChan inChan msg


-- | Receive a message. When the mailbox is empty, blocks until a message
-- arrives.
--
-- ===== __ Example __
--
-- > logger :: Process String ()
-- > logger = forever do
-- >   string <- receive
-- >   liftIO $ putStrLn string
--
-- @since 0.3.0.0
receive :: HasMsg msg => Process msg msg
receive = do
  Mailbox outChan <- Process $ asks mailbox
  liftIO $ Unagi.readChan outChan


-- | Try to receive a message. When the mailbox is empty, returns `Nothing`.
--
-- @since 0.3.0.0
tryReceive :: HasMsg msg => Process msg (Maybe msg)
tryReceive = do
  Mailbox outChan <- Process $ asks mailbox
  (element, _) <- liftIO $ Unagi.tryReadChan outChan
  liftIO $ Unagi.tryRead element


-- | TODO
--
-- @since 0.3.0.0
run :: (HasMsg msg, MonadIO m) => Process msg a -> m a
run process = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  runImpl address mailbox process


-- | More efficient version of `run`, for processes which receive no messages
-- (@msg ~ `NoMsg`@). See docs for `run` for more information.
--
-- @since 0.3.0.0
run_ :: MonadIO m => Process NoMsg a -> m a
run_ process = do
  let address = Address (error voidMsgError)
  let mailbox = Mailbox (error voidMsgError)
  runImpl address mailbox process


runImpl :: MonadIO m => Address msg -> Mailbox msg -> Process msg a -> m a
runImpl address mailbox process = do
  liftIO $ Ki.scoped \kiScope -> do
    let scope = Scope kiScope
    runProcess ProcessEnv{address, mailbox, scope} process


voidMsgError :: String
voidMsgError = unlines . fmap unwords $
  [ ["[!] drama internal error"]
  , []
  , [ "Attempted to use the address or mailbox of a process which cannot send"
    , "or receive messages (msg ~ NoMsg)."
    ]
  , [ "This should be impossible using non-internal modules!" ]
  , []
  , [ "Please report this issue at https://github.com/evanrelf/drama/issues"
    ]
  ]
