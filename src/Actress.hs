{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | Simple actor library for Haskell

module Actress
  ( -- * Defining actors
    Actor
  , new

    -- ** Managing state
  , loop

    -- * Spawning actors
  , Scope
  , spawn
  , wait

    -- * Messages

    -- ** Sending messages
  , Address
  , send

    -- ** Receiving messages
  , Mailbox
  , receive

    -- * Running your program
  , run
  )
where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Ki


newtype Actor msg = Actor
  { _unActor
      :: Address msg
      -> Mailbox msg
      -> Scope
      -> IO ()
  }


newtype Address msg = Address (Unagi.InChan msg)


newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


newtype Scope = Scope (Ki.Scope)


-- | Define a new actor.
--
-- Example:
--
-- > printer :: Actor String
-- > printer = new \self mailbox scope -> do
-- >   ...
--
new :: (Address msg -> Mailbox msg -> Scope -> IO ()) -> Actor msg
new = Actor


-- | Loop indefinitely with state. Looping stops when you return `Nothing`. Use
-- `forever` for stateless infinite loops.
--
-- Example:
--
-- > counter :: IO ()
-- > counter = loop (10 :: Int) \count -> do
-- >   print count
-- >   if count > 0
-- >     then pure $ Just (count - 1)
-- >     else pure Nothing
--
loop
  :: s
  -- ^ Initial state
  -> (s -> IO (Maybe s))
  -- ^ Action to perform, optionally returning a new state to continue looping
  -> IO ()
loop x0 k = do
  k x0 >>= \case
    Just x -> loop x k
    Nothing -> pure ()


-- | Spawn a new actor in the given scope. Returns the spawned actor's address.
--
-- Example:
--
-- > printerAddress <- spawn scope printer
--
spawn :: Scope -> Actor msg -> IO (Address msg)
spawn (Scope kiScope) (Actor actorFn) = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.fork_ kiScope (Ki.scoped \childKiScope -> actorFn address mailbox (Scope childKiScope))
  pure address


-- | Wait for all actors spawned in the given scope to terminate.
--
-- Example:
--
-- > fooAddress <- spawn scope foo
-- > barAddress <- spawn scope bar
-- > wait scope
--
wait :: Scope -> IO ()
wait (Scope kiScope) = Ki.wait kiScope


-- | Given an actor's address, send it a message.
--
-- Example:
--
-- > send printerAddress "Hello, world!"
--
send
  :: Address msg
  -- ^ Recipient actor's address
  -> msg
  -- ^ Message
  -> IO ()
send (Address inChan) msg = Unagi.writeChan inChan msg


-- | Receive a message sent to the actor's mailbox. This function blocks until
-- a message is received.
--
-- Example:
--
-- > printer :: Actor String
-- > printer = new \_self mailbox _scope -> forever do
-- >   string <- receive mailbox
-- >   putStrLn string
--
receive
  :: Mailbox msg
  -- ^ Actor's mailbox
  -> IO msg
  -- ^ Received message
receive (Mailbox outChan) = Unagi.readChan outChan


-- | Run a top-level actor. Intended to be used at the entry point of your
-- program.
run :: Actor msg -> IO ()
run (Actor actorFn) = do
  (inChan, outChan) <- Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan
  Ki.scoped \kiScope -> actorFn address mailbox (Scope kiScope)
