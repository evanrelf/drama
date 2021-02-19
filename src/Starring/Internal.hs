{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module:     Starring.Internal
-- Stability:  Experimental
-- License:    ISC
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com

module Starring.Internal where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Ki


-- | @since 0.1.0.0
newtype Address msg = Address (Unagi.InChan msg)


-- | @since 0.1.0.0
newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


-- | @since 0.1.0.0
newtype Scope = Scope (Ki.Scope)


-- | @since 0.1.0.0
data ActorEnv msg = ActorEnv
  { address :: Address msg
  , mailbox :: Mailbox msg
  , scope :: Scope
  }


-- | @since 0.1.0.0
newtype Actor msg a = Actor (ReaderT (ActorEnv msg) IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ActorEnv msg)
    )


-- | @since 0.1.0.0
runActor :: MonadIO m => ActorEnv msg -> Actor msg a -> m a
runActor actorEnv (Actor m) = liftIO $ runReaderT m actorEnv


-- | Loop indefinitely with state. Use `Control.Monad.forever` for stateless
-- infinite loops.
--
-- Example:
--
-- > counter :: Actor () Int
-- > counter = loop 10 \count -> do
-- >   liftIO $ print count
-- >   if count > 0
-- >     then continue (count - 1)
-- >     else exit count
--
-- @since 0.1.0.0
loop
  :: s
  -- ^ Initial state
  -> (s -> Actor msg (Either s a))
  -- ^ Action to perform, either returning a new state to continue looping, or
  -- a final value to stop looping.
  -> Actor msg a
loop s0 k =
  k s0 >>= \case
    Left s -> loop s k
    Right x -> pure x


-- | Continue looping with state.
--
-- prop> continue s = pure (Left s)
--
-- @since 0.1.0.0
continue :: s -> Actor msg (Either s a)
continue s = pure (Left s)


-- | Exit loop with value.
--
-- prop> exit x = pure (Right x)
--
-- @since 0.1.0.0
exit :: a -> Actor msg (Either s a)
exit x = pure (Right x)


-- | Spawn a new actor. Returns the spawned actor's address.
--
-- Example:
--
-- > printerAddress <- spawn printer
--
-- @since 0.1.0.0
spawn :: Actor childMsg () -> Actor msg (Address childMsg)
spawn actor = do
  (inChan, outChan) <- liftIO $ Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan

  Scope kiScope <- asks scope
  liftIO $ Ki.fork_ kiScope $ Ki.scoped \childKiScope ->
    let childScope = Scope childKiScope
        childEnv = ActorEnv{address, mailbox, scope = childScope}
     in runActor childEnv actor

  pure address


-- | Wait for all actors spawned by the current actor to terminate.
--
-- Example:
--
-- > fooAddress <- spawn foo
-- > barAddress <- spawn bar
-- > wait
--
-- @since 0.1.0.0
wait :: Actor msg ()
wait = do
  Scope kiScope <- asks scope
  liftIO $ Ki.wait kiScope


-- | Return the current actor's own address.
--
-- @since 0.1.0.0
here :: Actor msg (Address msg)
here = asks address


-- | Given an actor's address, send it a message.
--
-- Example:
--
-- > send printerAddress "Hello, world!"
--
-- @since 0.1.0.0
send
  :: Address recipientMsg
  -- ^ Recipient actor's address
  -> recipientMsg
  -- ^ Message
  -> Actor msg ()
send (Address inChan) msg = liftIO $ Unagi.writeChan inChan msg


-- | Receive a message sent to the actor's mailbox. This function blocks until
-- a message is received.
--
-- Example:
--
-- > printer :: Actor String ()
-- > printer = forever do
-- >   string <- receive
-- >   liftIO $ putStrLn string
--
-- @since 0.1.0.0
receive :: Actor msg msg
receive = do
  Mailbox outChan <- asks mailbox
  liftIO $ Unagi.readChan outChan


-- | Receive a message sent to the actor's mailbox. This function blocks until
-- a message is received.
--
-- Example:
--
-- > printer :: Actor String ()
-- > printer = forever do
-- >   tryReceive >>= \case
-- >     Just string -> liftIO $ putStrLn string
-- >     Nothing -> ...
--
-- @since 0.1.0.0
tryReceive :: Actor msg (Maybe msg)
tryReceive = do
  Mailbox outChan <- asks mailbox
  (element, _) <- liftIO $ Unagi.tryReadChan outChan
  liftIO $ Unagi.tryRead element


-- | Run a top-level actor. Intended to be used at the entry point of your
-- program.
--
-- @since 0.1.0.0
run :: MonadIO m => Actor msg a -> m a
run actor = do
  (inChan, outChan) <- liftIO $ Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan

  liftIO $ Ki.scoped \kiScope -> do
    let scope = Scope kiScope
    runActor ActorEnv{address, mailbox, scope} actor
