{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Concurrent.MVar (MVar)
import Control.Exception (finally)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (..), askUnliftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), ask, mapReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Zip (MonadZip)
import Data.Kind (Type)

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.MVar as MVar
import qualified Ki

-- Support `MonadFail` on GHC 8.6.5
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif
#if MIN_VERSION_base(4,13,0)
import Prelude hiding (MonadFail)
#endif


-- | Monad supporting actor operations.
--
-- @since TODO
newtype ActorT (msg :: Type -> Type) m a
  = ActorT { unActorT :: ReaderT (ActorEnv msg) m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , Alternative
    , MonadPlus
#if MIN_VERSION_base(4,9,0)
    , MonadFail
#endif
    , MonadFix
    , MonadZip
    , MonadTrans
    )


mapActorT :: (m a -> n b) -> ActorT msg m a -> ActorT msg n b
mapActorT f = ActorT . mapReaderT f . unActorT


instance MonadReader r m => MonadReader r (ActorT msg m) where
  ask = lift ask
  reader = lift . reader
  local = mapActorT . local


-- @since 0.4.0.0
type Actor msg = ActorT msg IO


-- | Ambient context provided by the `Actor` monad.
--
-- Values in `ActorEnv` are scoped to the current actor and cannot be safely
-- shared. Functions like `spawn`, `receive`, and `getSelf` use these values as
-- implicit parameters to avoid leaking internals (and for convenience).
--
-- @since 0.4.0.0
data ActorEnv msg = ActorEnv
  { address :: Address msg
    -- ^ Current actor's address.
  , mailbox :: Mailbox msg
    -- ^ Current actor's mailbox.
  , scope :: Ki.Scope
    -- ^ Current actor's token used for spawning threads. Delimits the lifetime
    -- of child actors (threads).
  }


-- | Address for sending messages to an actor. Obtained by running `spawn`,
-- `getSelf`, or `receive` (if another actor sends you an address).
--
-- @since 0.4.0.0
data Address msg = Address
  { channel :: Unagi.InChan (Envelope msg)
  , alive :: MVar ()
  }


-- | Mailbox where an actor receives messages. Cannot be shared with other
-- actors; used implicitly by `receive` and `tryReceive`.
--
-- @since 0.4.0.0
newtype Mailbox msg = Mailbox (Unagi.OutChan (Envelope msg))


-- | Wrapper around higher-kinded message types.
--
-- Higher-kinded message types are defined as GADTs with a type parameter. This
-- allows specifying the response type for messages.
--
-- @since 0.4.0.0
data Envelope (msg :: Type -> Type) where
  Cast :: msg () -> Envelope msg
  Call :: MVar res -> msg res -> Envelope msg


-- | Message type used by actors which do not receive messages.
--
-- @since 0.4.0.0
data NoMsg res


-- | @since TODO
type ActorT_ = ActorT NoMsg


-- | @since 0.4.0.0
type Actor_ = Actor NoMsg


-- | Spawn a child actor and return its address.
--
-- @since 0.4.0.0
spawn
  :: MonadUnliftIO m
  => ActorT msg m ()
  -- ^ Actor to spawn
  -> ActorT _msg m (Address msg)
  -- ^ Spawned actor's address
spawn actor = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let channel = inChan
  alive <- liftIO MVar.newEmptyMVar
  let address = Address{channel, alive}
  let mailbox = Mailbox outChan
  spawnImpl address mailbox actor
  pure address


-- | More efficient version of `spawn`, for actors which receive no messages
-- (@msg ~ `NoMsg`@). See docs for `spawn` for more information.
--
-- @since 0.4.0.0
spawn_ :: MonadUnliftIO m => ActorT_ m () -> ActorT msg m ()
spawn_ actor = do
  let channel = error noMsgError
  alive <- liftIO MVar.newEmptyMVar
  let address = Address{channel, alive}
  let mailbox = Mailbox (error noMsgError)
  spawnImpl address mailbox actor


spawnImpl
  :: MonadUnliftIO m
  => Address msg
  -> Mailbox msg
  -> ActorT msg m ()
  -> ActorT _msg m ()
spawnImpl address mailbox actor = do
  UnliftIO unliftIO <- lift askUnliftIO
  ActorEnv{scope} <- ActorT ask
  liftIO $ Ki.fork_ scope $ unliftIO $ runActorTImpl address mailbox actor


-- | Check whether an actor is still running.
--
-- @since TODO
isAlive :: MonadIO m => Address msg -> ActorT _msg m Bool
isAlive Address{alive} = liftIO $ MVar.isEmptyMVar alive


-- | Block until child actor has terminated.
--
-- @since TODO
wait :: MonadIO m => Address msg -> ActorT _msg m ()
wait Address{alive} = liftIO $ MVar.readMVar alive


-- | Block until all child actors have terminated.
--
-- @since TODO
waitAll :: MonadIO m => ActorT msg m ()
waitAll = do
  ActorEnv{scope} <- ActorT ask
  liftIO $ Ki.wait scope


-- | Return the current actor's address.
--
-- @since 0.4.0.0
getSelf :: Monad m => ActorT msg m (Address msg)
getSelf = do
  ActorEnv{address} <- ActorT ask
  pure address


-- | Send a message to another actor, expecting no response. Returns immediately
-- without blocking.
--
-- @since 0.4.0.0
cast
  :: MonadIO m
  => Address msg
  -- ^ Actor's address
  -> msg ()
  -- ^ Message to send
  -> ActorT _msg m ()
cast Address{channel = inChan} msg = liftIO $ Unagi.writeChan inChan (Cast msg)


-- | Send a message to another actor, and wait for a response.
--
-- @since 0.4.0.0
call
  :: MonadIO m
  => Address msg
  -- ^ Actor's address
  -> msg res
  -- ^ Message to send
  -> ActorT _msg m res
  -- ^ Response
call Address{channel = inChan} msg = liftIO do
  resMVar <- MVar.newEmptyMVar
  Unagi.writeChan inChan (Call resMVar msg)
  MVar.takeMVar resMVar


-- | Receive a message. When the mailbox is empty, blocks until a message
-- arrives.
--
-- @since 0.4.0.0
receive
  :: MonadIO m
  => (forall res. msg res -> ActorT msg m res)
  -- ^ Callback function that responds to messages
  -> ActorT msg m ()
receive callback = do
  ActorEnv{mailbox = Mailbox outChan} <- ActorT ask
  envelope <- liftIO $ Unagi.readChan outChan
  case envelope of
    Cast msg ->
      callback msg
    Call resMVar msg -> do
      res <- callback msg
      liftIO $ MVar.putMVar resMVar res


-- | Try to receive a message. When the mailbox is empty, returns immediately.
--
-- @since 0.4.0.0
tryReceive
  :: MonadIO m
  => (forall res. msg res -> ActorT msg m res)
  -- ^ Callback function that responds to messages
  -> ActorT msg m Bool
tryReceive callback = do
  ActorEnv{mailbox = Mailbox outChan} <- ActorT ask
  (element, _) <- liftIO $ Unagi.tryReadChan outChan
  envelope <- liftIO $ Unagi.tryRead element
  case envelope of
    Nothing ->
      pure False
    Just (Cast msg) -> do
      callback msg
      pure True
    Just (Call resMVar msg) -> do
      res <- callback msg
      liftIO $ MVar.putMVar resMVar res
      pure True


-- | Run a top-level actor. Intended to be used at the entry point of your
-- program.
--
-- @since TODO
runActorT :: MonadUnliftIO m => ActorT msg m a -> m a
runActorT actor = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let channel = inChan
  alive <- liftIO MVar.newEmptyMVar
  let address = Address{channel, alive}
  let mailbox = Mailbox outChan
  runActorTImpl address mailbox actor


-- | More efficient version of `runActorT`, for actors which receive no messages
-- (@msg ~ `NoMsg`@). See docs for `runActorT` for more information.
--
-- @since TODO
runActorT_ :: MonadUnliftIO m => ActorT msg m a -> m a
runActorT_ actor = do
  let channel = error noMsgError
  alive <- liftIO MVar.newEmptyMVar
  let address = Address{channel, alive}
  let mailbox = Mailbox (error noMsgError)
  runActorTImpl address mailbox actor


-- @since 0.4.0.0
runActor :: MonadIO m => Actor msg a -> m a
runActor = liftIO . runActorT


-- @since 0.4.0.0
runActor_ :: MonadIO m => Actor_ a -> m a
runActor_ = liftIO . runActorT_


runActorTImpl
  :: MonadUnliftIO m
  => Address msg
  -> Mailbox msg
  -> ActorT msg m a
  -> m a
runActorTImpl address@Address{alive} mailbox actor = do
  UnliftIO unliftIO <- askUnliftIO
  liftIO $ Ki.scoped \scope ->
    (unliftIO $ runReaderT (unActorT actor) ActorEnv{address, mailbox, scope})
      `finally` (MVar.tryPutMVar alive ())


noMsgError :: String
noMsgError = unlines . fmap unwords $
  [ ["[!] drama internal error"]
  , []
  , [ "Attempted to use the address or mailbox of a actor which cannot send"
    , "or receive messages (msg ~ NoMsg)."
    ]
  , [ "This should be impossible using non-internal modules!" ]
  , []
  , [ "Please report this issue at https://github.com/evanrelf/drama/issues"
    ]
  ]
