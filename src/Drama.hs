-- |
-- Module:     Drama
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2022 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Actor library for Haskell
--
-- ===== Example
--
-- An actor which encapsulates a piece of mutable state. Its @StateMsg@ type
-- specifies which messages it accepts, which messages return a response, and
-- what type that response is.
--
-- > data StateMsg s res where
-- >   GetState :: StateMsg s s
-- >   GetsState :: (s -> a) -> StateMsg s a
-- >   PutState :: s -> StateMsg s ()
-- >   ModifyState :: (s -> s) -> StateMsg s ()
-- >
-- > state :: s -> Actor (StateMsg s) ()
-- > state s0 = do
-- >   stateIORef <- liftIO $ newIORef s0
-- >
-- >   forever $ receive \case
-- >     GetState ->
-- >       liftIO $ readIORef stateIORef
-- >
-- >     GetsState f -> do
-- >       s <- liftIO $ readIORef stateIORef
-- >       pure (f s)
-- >
-- >     PutState s ->
-- >       liftIO $ writeIORef stateIORef s
-- >
-- >     ModifyState f ->
-- >       liftIO $ modifyIORef stateIORef f

module Drama
  ( Actor
  , ActorT
  , MonadActor

    -- * Spawning actors
  , spawn
  , wait

    -- * Sending messages
  , Address
  , cast
  , call
  , getSelf

    -- * Receiving messages
  , receive
  , tryReceive

    -- * Running your program
  , runActor
  , runActorT
  , mapActorT

    -- * Not receiving messages
  , Actor_
  , NoMsg
  , spawn_
  , runActor_
  , runActorT_
  )
where

import Drama.Internal
