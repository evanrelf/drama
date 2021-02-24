-- |
-- Module:     Drama.Server
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Higher-level process operations, allowing responses to messages. Inspired by
-- Elixir and Erlang's generic servers (@GenServer@ / @gen_server@).
--
-- ===== __Example__
--
-- A server which encapsulates a piece of mutable state. Its @StateMsg@ type
-- specifies which messages it accepts, which messages return a response, and
-- what type that response is.
--
-- > data StateMsg s res where
-- >   GetState :: StateMsg s s
-- >   GetsState :: (s -> a) -> StateMsg s a
-- >   PutState :: s -> StateMsg s ()
-- >   ModifyState :: (s -> s) -> StateMsg s ()
-- >
-- > state :: s -> Server (StateMsg s) ()
-- > state s0 = do
-- >   stateIORef <- liftIO $ newIORef s0
-- >
-- >   forever $ receive >>= handle \case
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

module Drama.Server
  ( Server
  , Envelope

    -- * Sending messages
  , cast
  , call

    -- * Handling messages
  , handle
  )
where

import Drama.Server.Internal
