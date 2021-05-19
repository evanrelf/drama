{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- For `HasMsg msg`
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module:     Drama.Internal
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com

module Drama.Internal where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Data.Kind (Constraint, Type)
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


-- | Monad supporting actor operations. Inspired by Elixir and Erlang's
-- processes.
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


-- | @since 0.3.0.0
type Server msg a = Process (Envelope msg) a


-- | Provided some `ProcessEnv`, convert a `Process` action into an `IO`
-- action.
--
-- @since 0.3.0.0
runProcess :: MonadIO m => ProcessEnv msg -> Process msg a -> m a
runProcess processEnv (Process reader) = liftIO $ runReaderT reader processEnv


-- | Ambient context provided by the `Process` monad.
--
-- Values in `ProcessEnv` are scoped to the current process and cannot be safely
-- shared. Functions like `spawn`, `receive`, and `here` use these values as
-- implicit parameters to avoid leaking internals (and for convenience).
--
-- @since 0.3.0.0
data ProcessEnv msg = ProcessEnv
  { address :: Address msg
    -- ^ Current process' address.
  , mailbox :: Mailbox msg
    -- ^ Current process' mailbox.
  , scope :: Scope
    -- ^ Current process' token used for spawning threads.
  }


-- | Address for sending messages to a process. Obtained by running `spawn`,
-- `here`, or `receive` (if another process sends you an address).
--
-- @since 0.3.0.0
newtype Address msg = Address (Unagi.InChan msg)


-- | Mailbox where a process receives messages. Cannot be shared with other
-- processes; used implicitly by `receive` and `tryReceive`.
--
-- @since 0.3.0.0
newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


-- | Token delimiting the lifetime of child processes (threads) created by a
-- process.
--
-- @since 0.3.0.0
newtype Scope = Scope Ki.Scope


-- | Constraint which prevents setting `msg ~ Void`, and provides helpful type
-- errors.
--
-- @since 0.3.0.0
type family HasMsg msg :: Constraint where
  HasMsg NoMsg = TypeError ('Text "Processes with 'msg ~ NoMsg' cannot receive messages")
  HasMsg Void = TypeError ('Text "Use 'msg ~ NoMsg' instead of 'msg ~ Void' for processes which do not receive messages")
  HasMsg msg = ()


-- | Message type used by processes which do not receive messages.
--
-- @since 0.3.0.0
data NoMsg


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


-- | Spawn a child process and return its address.
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


-- | Receive a message. When the mailbox is empty, blocks until a message
-- arrives.
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


-- | Run a top-level process. Intended to be used at the entry point of your
-- program.
--
-- If your program is designed with processes in mind, you can use `Process` as
-- your program's base monad:
--
-- > main :: IO ()
-- > main = run do
-- >   ...
--
-- Otherwise, use `run` like you would with @run@ functions from libraries like
-- @transformers@ or @mtl@.
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
