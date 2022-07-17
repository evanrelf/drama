{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module:     Drama.Internal
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2022 Evan Relf
-- Maintainer: evan@evanrelf.com

module Drama.Internal where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (MonadPlus, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT (..), asks, mapReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Kind (Type)

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.STM as STM
import qualified Ki.Unlifted as Ki

-- Support `MonadFail` on GHC 8.6.5
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail)
#endif
#if MIN_VERSION_base(4,13,0)
import Prelude hiding (MonadFail)
#endif


-- | Monad supporting actor operations.
--
-- @since 0.4.0.0
type Actor msg = ActorT msg IO


-- | Monad transformer supporting actor operations.
--
-- @since 0.6.0.0
newtype ActorT (msg :: Type -> Type) m a = ActorT (ReaderT (ActorEnv msg) m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadUnliftIO -- ^ @since 0.5.0.0
    , Alternative
    , MonadPlus
#if MIN_VERSION_base(4,9,0)
    , MonadFail
#endif
    , MonadFix
    )


-- | Transform the computation inside an `ActorT`.
--
-- @since 0.6.0.0
mapActorT :: (m a -> n b) -> ActorT msg m a -> ActorT msg n b
mapActorT f (ActorT reader) = ActorT $ mapReaderT f reader


-- | Ambient context provided by the `ActorT` monad transformer.
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
newtype Address msg = Address (Unagi.InChan (Envelope msg))


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


-- | @since 0.4.0.0
type Actor_ = Actor NoMsg


-- | @since 0.6.0.0
type ActorT_ = ActorT NoMsg


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
  let address = Address inChan
  let mailbox = Mailbox outChan
  spawnImpl address mailbox actor
  pure address


-- | More efficient version of `spawn`, for actors which receive no messages
-- (@msg ~ `NoMsg`@). See docs for `spawn` for more information.
--
-- @since 0.4.0.0
spawn_ :: MonadUnliftIO m => ActorT_ m () -> ActorT msg m ()
spawn_ actor = do
  let address = Address (error noMsgError)
  let mailbox = Mailbox (error noMsgError)
  spawnImpl address mailbox actor


spawnImpl
  :: MonadUnliftIO m
  => Address msg
  -> Mailbox msg
  -> ActorT msg m ()
  -> ActorT _msg m ()
spawnImpl address mailbox actor = do
  scope <- ActorT $ asks scope
  void $ Ki.fork scope $ lift $ runActorTImpl address mailbox actor


-- | Block until all child actors have terminated.
--
-- @since 0.4.0.0
wait :: MonadIO m => ActorT msg m ()
wait = do
  scope <- ActorT $ asks scope
  liftIO $ STM.atomically $ Ki.awaitAll scope


-- | Return the current actor's address.
--
-- @since 0.4.0.0
getSelf :: Monad m => ActorT msg m (Address msg)
getSelf = ActorT $ asks address


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
cast (Address inChan) msg = liftIO $ Unagi.writeChan inChan (Cast msg)


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
call (Address inChan) msg = liftIO do
  resMVar <- newEmptyMVar
  Unagi.writeChan inChan (Call resMVar msg)
  takeMVar resMVar


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
  Mailbox outChan <- ActorT $ asks mailbox
  envelope <- liftIO $ Unagi.readChan outChan
  case envelope of
    Cast msg ->
      callback msg
    Call resMVar msg -> do
      res <- callback msg
      liftIO $ putMVar resMVar res


-- | Try to receive a message. When the mailbox is empty, returns immediately.
--
-- @since 0.4.0.0
tryReceive
  :: MonadIO m
  => (forall res. msg res -> ActorT msg m res)
  -- ^ Callback function that responds to messages
  -> ActorT msg m Bool
tryReceive callback = do
  Mailbox outChan <- ActorT $ asks mailbox
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
      liftIO $ putMVar resMVar res
      pure True


-- | See docs for `runActorT` for more information.
--
-- @since 0.4.0.0
runActor :: MonadIO m => Actor msg a -> m a
runActor actor = liftIO $ runActorT actor


-- | See docs for `runActorT_` for more information.
--
-- @since 0.4.0.0
runActor_ :: MonadIO m => Actor_ a -> m a
runActor_ actor = liftIO $ runActorT_ actor


-- | Run a top-level actor. Intended to be used at the entry point of your
-- program.
--
-- If your program is designed with actors in mind, you can use `Actor` as
-- your program's base monad:
--
-- > main :: IO ()
-- > main = runActorT root
-- >
-- > root :: Actor RootMsg ()
-- > root = do
-- >   ...
--
-- Otherwise, use `runActorT` like you would with @run@ functions from libraries
-- like @transformers@ or @mtl@.
--
-- @since 0.6.0.0
runActorT :: MonadUnliftIO m => ActorT msg m a -> m a
runActorT actor = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  runActorTImpl address mailbox actor


-- | More efficient version of `runActorT`, for actors which receive no messages
-- (@msg ~ `NoMsg`@). See docs for `runActorT` for more information.
--
-- @since 0.6.0.0
runActorT_ :: MonadUnliftIO m => ActorT_ m a -> m a
runActorT_ actor = do
  let address = Address (error noMsgError)
  let mailbox = Mailbox (error noMsgError)
  runActorTImpl address mailbox actor


runActorTImpl
  :: MonadUnliftIO m
  => Address msg
  -> Mailbox msg
  -> ActorT msg m a
  -> m a
runActorTImpl address mailbox (ActorT reader) =
  Ki.scoped \scope -> runReaderT reader ActorEnv{address, mailbox, scope}


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
