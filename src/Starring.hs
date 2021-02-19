{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Simple actor library for Haskell
--
-- ==== __Example__
--
-- Fizz buzz, using three actors: `main`, `logger`, and `fizzBuzz`:
--
-- > {-# LANGUAGE BlockArguments #-}
-- > {-# LANGUAGE MultiWayIf #-}
-- > {-# LANGUAGE NumericUnderscores #-}
-- >
-- > module Main (main) where
-- >
-- > import Control.Concurrent (threadDelay)
-- > import Control.Monad (forever)
-- > import Control.Monad.IO.Class (MonadIO (..))
-- > import Starring
-- > import Prelude hiding (log)
-- >
-- > main :: IO ()
-- > main = run do
-- >   loggerAddress <- spawn logger
-- >   _ <- spawn (fizzBuzz loggerAddress)
-- >   wait
-- >
-- > logger :: Actor String ()
-- > logger = forever do
-- >   string <- receive
-- >   liftIO $ putStrLn string
-- >
-- > fizzBuzz :: Address String -> Actor () ()
-- > fizzBuzz loggerAddress = do
-- >   let log = send loggerAddress
-- >
-- >   loop (0 :: Int) \n -> do
-- >     if | n `mod` 15 == 0 -> log "FizzBuzz"
-- >        | n `mod`  3 == 0 -> log "Fizz"
-- >        | n `mod`  5 == 0 -> log "Buzz"
-- >        | otherwise       -> log (show n)
-- >
-- >     liftIO $ threadDelay 500_000
-- >
-- >     continue (n + 1)
--
-- Output:
--
-- > λ> main
-- > 1
-- > 2
-- > Fizz
-- > 4
-- > Buzz
-- > Fizz
-- > 7
-- > 8
-- > Fizz
-- > Buzz
-- > 11
-- > Fizz
-- > 13
-- > 14
-- > FizzBuzz
-- > ...

module Starring
  ( Actor

    -- * Spawning actors
  , spawn
  , wait

    -- * Messages

    -- ** Addresses
  , Address
  , here

    -- ** Sending messages
  , send

    -- ** Receiving messages
  , receive
  , tryReceive

    -- * Managing state
  , loop
  , continue
  , exit

    -- * Running your program
  , run
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Ki


newtype Address msg = Address (Unagi.InChan msg)


newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


newtype Scope = Scope (Ki.Scope)


data ActorEnv msg = ActorEnv
  { address :: Address msg
  , mailbox :: Mailbox msg
  , scope :: Scope
  }


newtype Actor msg a = Actor (ReaderT (ActorEnv msg) IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ActorEnv msg)
    )


runActor :: MonadIO m => ActorEnv msg -> Actor msg a -> m a
runActor actorEnv (Actor m) = liftIO $ runReaderT m actorEnv


-- | Loop indefinitely with state. Use `forever` for stateless infinite loops.
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
-- > continue s = pure (Left s)
--
continue :: s -> Actor msg (Either s a)
continue s = pure (Left s)


-- | Exit loop with value.
--
-- > exit x = pure (Right x)
--
exit :: a -> Actor msg (Either s a)
exit x = pure (Right x)


-- | Spawn a new actor. Returns the spawned actor's address.
--
-- Example:
--
-- > printerAddress <- spawn printer
--
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
wait :: Actor msg ()
wait = do
  Scope kiScope <- asks scope
  liftIO $ Ki.wait kiScope


-- | Return the current actor's own address.
here :: Actor msg (Address msg)
here = asks address


-- | Given an actor's address, send it a message.
--
-- Example:
--
-- > send printerAddress "Hello, world!"
--
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
tryReceive :: Actor msg (Maybe msg)
tryReceive = do
  Mailbox outChan <- asks mailbox
  (element, _) <- liftIO $ Unagi.tryReadChan outChan
  liftIO $ Unagi.tryRead element


-- | Run a top-level actor. Intended to be used at the entry point of your
-- program.
run :: MonadIO m => Actor msg a -> m a
run actor = do
  (inChan, outChan) <- liftIO $ Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan

  liftIO $ Ki.scoped \kiScope -> do
    let scope = Scope kiScope
    runActor ActorEnv{address, mailbox, scope} actor
