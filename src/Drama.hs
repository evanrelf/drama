-- |
-- Module:     Drama
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2022 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Actor library for Haskell
--
-- ===== __Example__
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
-- > state = loop
-- >   where
-- >     loop s = do
-- >       s' <- receive \case
-- >         GetState -> pure (s, s)
-- >         GetsState f -> pure (f s, s)
-- >         PutState s' -> pure ((), s')
-- >         ModifyState f -> pure ((), f s)
-- >       loop s'

module Drama
  ( Actor

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
  , receive_
  , tryReceive
  , tryReceive_

    -- * Running your program
  , runActor

    -- * Not receiving messages
  , Actor_
  , NoMsg
  , spawn_
  , runActor_
  )
where

import Drama.Internal
