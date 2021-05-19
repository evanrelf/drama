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


-- | Monad supporting actor operations.
--
-- @since 0.4.0.0
newtype Actor (msg :: Type -> Type) a = Actor (ReaderT (ActorEnv msg) IO a)
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


-- | Spawn a child actor and return its address.
--
-- @since 0.4.0.0
spawn
  :: Actor msg ()
  -- ^ Actor to spawn
  -> Actor _msg (Address msg)
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
spawn_ :: Actor_ () -> Actor msg ()
spawn_ actor = do
  let address = Address (error noMsgError)
  let mailbox = Mailbox (error noMsgError)
  spawnImpl address mailbox actor


spawnImpl
  :: Address msg
  -> Mailbox msg
  -> Actor msg ()
  -> Actor _msg ()
spawnImpl address mailbox actor = do
  scope <- Actor $ asks scope
  liftIO $ Ki.fork_ scope $ runActorImpl address mailbox actor


-- | Block until all child actors have terminated.
--
-- @since 0.4.0.0
wait :: Actor msg ()
wait = do
  scope <- Actor $ asks scope
  liftIO $ Ki.wait scope


-- | Return the current actor's address.
--
-- @since 0.4.0.0
getSelf :: Actor msg (Address msg)
getSelf = Actor $ asks address


-- | Send a message to another actor, expecting no response. Returns immediately
-- without blocking.
--
-- @since 0.4.0.0
cast
  :: Address msg
  -- ^ Actor's address
  -> msg ()
  -- ^ Message to send
  -> Actor _msg ()
cast (Address inChan) msg = liftIO $ Unagi.writeChan inChan (Cast msg)


-- | Send a message to another actor, and wait for a response.
--
-- @since 0.4.0.0
call
  :: Address msg
  -- ^ Actor's address
  -> msg res
  -- ^ Message to send
  -> Actor _msg res
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
  :: (forall res. msg res -> Actor msg res)
  -- ^ Callback function that responds to messages
  -> Actor msg ()
receive callback = do
  Mailbox outChan <- Actor $ asks mailbox
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
  :: (forall res. msg res -> Actor msg res)
  -- ^ Callback function that responds to messages
  -> Actor msg Bool
tryReceive callback = do
  Mailbox outChan <- Actor $ asks mailbox
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


-- | Run a top-level actor. Intended to be used at the entry point of your
-- program.
--
-- If your program is designed with actors in mind, you can use `Actor` as
-- your program's base monad:
--
-- > main :: IO ()
-- > main = runActor root
-- >
-- > root :: Actor RootMsg ()
-- > root = do
-- >   ...
--
-- Otherwise, use `runActor` like you would with @run@ functions from libraries
-- like @transformers@ or @mtl@.
--
-- @since 0.4.0.0
runActor :: MonadIO m => Actor msg a -> m a
runActor actor = do
  (inChan, outChan) <- liftIO Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  runActorImpl address mailbox actor


-- | More efficient version of `runActor`, for actors which receive no messages
-- (@msg ~ `NoMsg`@). See docs for `runActor` for more information.
--
-- @since 0.4.0.0
runActor_ :: MonadIO m => Actor_ a -> m a
runActor_ actor = do
  let address = Address (error noMsgError)
  let mailbox = Mailbox (error noMsgError)
  runActorImpl address mailbox actor


runActorImpl :: MonadIO m => Address msg -> Mailbox msg -> Actor msg a -> m a
runActorImpl address mailbox (Actor reader) =
  liftIO $ Ki.scoped \scope ->
    runReaderT reader ActorEnv{address, mailbox, scope}


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
