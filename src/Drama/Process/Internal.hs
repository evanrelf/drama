{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- For `NotVoid msg`
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# OPTIONS_HADDOCK not-home #-}

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


-- | Forbid use of functions which use the underlying `Unagi.Chan` when a
-- process' message type is `()` or `Void`.
--
-- Forces users to use the more efficient `spawn_` and `run_` functions, and
-- prevents runtime exceptions.
--
-- @since 1.0.0.0
type family NotVoid msg :: Constraint where
  NotVoid Void = TypeError ('Text "Processes with 'msg ~ Void' cannot receive messages")
  NotVoid () = TypeError ('Text "Use 'msg ~ Void' instead of 'msg ~ ()' for processes which do not receive messages")
  NotVoid msg = ()


-- | The `Process` monad, where you can `spawn` other processes, and `send` and
-- `receive` messages.
--
-- @since 1.0.0.0
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


-- | @since 1.0.0.0
processToIO :: MonadIO m => ProcessEnv msg -> Process msg a -> m a
processToIO processEnv (Process reader) = liftIO $ runReaderT reader processEnv


-- | Environment for the `Process` monad.
--
-- @since 1.0.0.0
data ProcessEnv msg = ProcessEnv
  { address :: Address msg
  , mailbox :: Mailbox msg
  , scope :: Scope
  }


-- | The address for a process. Returned after `spawn`ing a process or asking
-- for the current process' address with `here`. Used to `send` messages to
-- specific processes.
--
-- @since 1.0.0.0
newtype Address msg = Address (Unagi.InChan msg)


-- | Where messages are delivered. Implicitly provided to `receive` and
-- `tryReceive` by the `Process` monad.
--
-- @since 1.0.0.0
newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


-- | @since 1.0.0.0
newtype Scope = Scope Ki.Scope


-- | Spawn a new process. Returns the spawned process' address.
--
-- Example:
--
-- > printerAddress <- spawn printer
--
-- @since 1.0.0.0
spawn
  :: NotVoid childMsg
  => Process childMsg ()
  -> Process msg (Address childMsg)
spawn process = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan

  Scope kiScope <- Process $ asks scope
  liftIO $ Ki.fork_ kiScope $ Ki.scoped \childKiScope ->
    let childScope = Scope childKiScope
        childEnv = ProcessEnv{address, mailbox, scope = childScope}
     in processToIO childEnv process

  pure address


-- | More efficient version of `spawn`, for processes which receive no messages
-- (@msg ~ `Void`@). See docs for `spawn` for more information.
--
-- @since 1.0.0.0
spawn_ :: Process Void () -> Process msg ()
spawn_ process = do
  let address = Address (error "unreachable")
  let mailbox = Mailbox (error "unreachable")

  Scope kiScope <- Process $ asks scope
  liftIO $ Ki.fork_ kiScope $ Ki.scoped \childKiScope ->
    let childScope = Scope childKiScope
        childEnv = ProcessEnv{address, mailbox, scope = childScope}
     in processToIO childEnv process


-- | Wait for all processes spawned by the current process to terminate.
--
-- Example:
--
-- > fooAddr <- spawn foo
-- > barAddr <- spawn bar
-- > wait
--
-- @since 1.0.0.0
wait :: Process msg ()
wait = do
  Scope kiScope <- Process $ asks scope
  liftIO $ Ki.wait kiScope


-- | Return the current process' own address. Useful for sending your address to
-- other processes, or for sending yourself a message.
--
-- @since 1.0.0.0
here :: NotVoid msg => Process msg (Address msg)
here = Process $ asks address


-- | Given a process' address, send it a message.
--
-- Example:
--
-- > send printerAddress "Hello, world!"
--
-- @since 1.0.0.0
send
  :: NotVoid recipientMsg
  => Address recipientMsg
  -> recipientMsg
  -> Process msg ()
send (Address inChan) msg = liftIO $ Unagi.writeChan inChan msg


-- | Receive a message sent to the process' mailbox. This function blocks until
-- a message is received.
--
-- Example:
--
-- > printer :: Process String ()
-- > printer = forever do
-- >   string <- receive
-- >   liftIO $ putStrLn string
--
-- @since 1.0.0.0
receive :: NotVoid msg => Process msg msg
receive = do
  Mailbox outChan <- Process $ asks mailbox
  liftIO $ Unagi.readChan outChan


-- | Receive a message sent to the process' mailbox. This function blocks until
-- a message is received.
--
-- Example:
--
-- > printer :: Process String ()
-- > printer = forever do
-- >   tryReceive >>= \case
-- >     Just string -> liftIO $ putStrLn string
-- >     Nothing -> ...
--
-- @since 1.0.0.0
tryReceive :: NotVoid msg => Process msg (Maybe msg)
tryReceive = do
  Mailbox outChan <- Process $ asks mailbox
  (element, _) <- liftIO $ Unagi.tryReadChan outChan
  liftIO $ Unagi.tryRead element


-- | Run a top-level process. Intended to be used at the entry point of your
-- program.
--
-- @since 1.0.0.0
run :: (NotVoid msg, MonadIO m) => Process msg a -> m a
run process = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan

  liftIO $ Ki.scoped \kiScope -> do
    let scope = Scope kiScope
    processToIO ProcessEnv{address, mailbox, scope} process


-- | More efficient version of `run`, for processes which receive no messages
-- (@msg ~ `Void`@). See docs for `run` for more information.
--
-- @since 1.0.0.0
run_ :: MonadIO m => Process Void a -> m a
run_ process = do
  let address = Address (error "unreachable")
  let mailbox = Mailbox (error "unreachable")

  liftIO $ Ki.scoped \kiScope -> do
    let scope = Scope kiScope
    processToIO ProcessEnv{address, mailbox, scope} process


-- | Loop indefinitely with state. Use `Control.Monad.forever` for stateless
-- infinite loops.
--
-- Example:
--
-- > counter :: Process () Int
-- > counter = loop 10 \count -> do
-- >   liftIO $ print count
-- >   if count > 0
-- >     then continue (count - 1)
-- >     else exit count
--
-- @since 1.0.0.0
loop
  :: Monad m
  => s
  -- ^ Initial state
  -> (s -> m (Either s a))
  -- ^ Action to perform, either returning a new state to continue looping, or
  -- a final value to stop looping.
  -> m a
loop s0 k =
  k s0 >>= \case
    Left s -> loop s k
    Right x -> pure x


-- | Continue looping with state.
--
-- prop> continue s = pure (Left s)
--
-- @since 1.0.0.0
continue :: Monad m => s -> m (Either s a)
continue s = pure (Left s)


-- | Exit loop with value.
--
-- prop> exit x = pure (Right x)
--
-- @since 1.0.0.0
exit :: Monad m => a -> m (Either s a)
exit x = pure (Right x)
