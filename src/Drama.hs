-- |
-- Module:     Drama
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Simple actor library for Haskell
--

module Drama
  ( Actor

    -- * Spawning actors
  , spawn
  , spawn_
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
  , run_

  -- * Re-exports
  , module Drama.Reexports
  )
where

import Drama.Internal
import Drama.Reexports
