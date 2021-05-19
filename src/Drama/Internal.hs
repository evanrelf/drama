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
import Data.Kind (Type)

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
newtype Process (msg :: Type -> Type) a = Process (ReaderT (ProcessEnv msg) IO a)
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


-- | Wrapper around higher-kinded message types, to make them compatible with
-- the lower-level `Process` machinery.
--
-- Higher-kinded message types are defined as GADTs with a type parameter. This
-- allows specifying the response type for messages.
--
-- @since 0.3.0.0
data Envelope (msg :: Type -> Type) where
  Cast :: msg () -> Envelope msg
  Call :: MVar res -> msg res -> Envelope msg


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
newtype Address msg = Address (Unagi.InChan (Envelope msg))


-- | Mailbox where a process receives messages. Cannot be shared with other
-- processes; used implicitly by `receive` and `tryReceive`.
--
-- @since 0.3.0.0
newtype Mailbox msg = Mailbox (Unagi.OutChan (Envelope msg))


-- | Token delimiting the lifetime of child processes (threads) created by a
-- process.
--
-- @since 0.3.0.0
newtype Scope = Scope Ki.Scope


-- | Message type used by processes which do not receive messages.
--
-- @since 0.3.0.0
data NoMsg res


-- | Spawn a child process and return its address.
--
-- @since 0.3.0.0
spawn
  :: Process msg ()
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
  let address = Address (error noMsgError)
  let mailbox = Mailbox (error noMsgError)
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
here :: Process msg (Address msg)
here = Process $ asks address


-- | Send a message to another process, expecting no response. Returns
-- immediately without blocking.
--
-- @since 0.3.0.0
cast
  :: Address msg
  -- ^ Process' address
  -> msg ()
  -- ^ Message to send
  -> Process _msg ()
cast (Address inChan) msg = liftIO $ Unagi.writeChan inChan (Cast msg)


-- | Send a message to another process, and wait for a response.
--
-- @since 0.3.0.0
call
  :: Address msg
  -- ^ Process' address
  -> msg res
  -- ^ Message to send
  -> Process _msg res
  -- ^ Response
call (Address inChan) msg = liftIO do
  resMVar <- newEmptyMVar
  Unagi.writeChan inChan (Call resMVar msg)
  takeMVar resMVar


-- | Receive a message. When the mailbox is empty, blocks until a message
-- arrives.
--
-- @since 0.3.0.0
receive
  :: (forall res. msg res -> Process msg res)
  -- ^ Callback function that responds to messages
  -> Process msg ()
receive callback = do
  Mailbox outChan <- Process $ asks mailbox
  envelope <- liftIO $ Unagi.readChan outChan
  case envelope of
    Cast msg ->
      callback msg
    Call resMVar msg -> do
      res <- callback msg
      liftIO $ putMVar resMVar res


-- | Try to receive a message. When the mailbox is empty, returns immediately.
--
-- @since 0.3.0.0
tryReceive
  :: (forall res. msg res -> Process msg res)
  -- ^ Callback function that responds to messages
  -> Process msg ()
tryReceive callback = do
  Mailbox outChan <- Process $ asks mailbox
  (element, _) <- liftIO $ Unagi.tryReadChan outChan
  envelope <- liftIO $ Unagi.tryRead element
  case envelope of
    Nothing ->
      pure ()
    Just (Cast msg) ->
      callback msg
    Just (Call resMVar msg) -> do
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
run :: MonadIO m => Process msg a -> m a
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
  let address = Address (error noMsgError)
  let mailbox = Mailbox (error noMsgError)
  runImpl address mailbox process


runImpl :: MonadIO m => Address msg -> Mailbox msg -> Process msg a -> m a
runImpl address mailbox process = do
  liftIO $ Ki.scoped \kiScope -> do
    let scope = Scope kiScope
    runProcess ProcessEnv{address, mailbox, scope} process


noMsgError :: String
noMsgError = unlines . fmap unwords $
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
