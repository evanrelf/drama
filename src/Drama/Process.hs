-- |
-- Module:     Drama.Process
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- TODO
--

module Drama.Process
  ( Process

    -- * Spawning processes
  , spawn
  , spawn_
  , wait

    -- * Messages

    -- ** High-level API
  , cast
  , call
  , handle

    -- ** Low-level API
  , send
  , receive
  , tryReceive

    -- ** Addresses
  , Address
  , here

    -- ** Message types
  , HasMsg
  , NoMsg
  , Envelope

    -- * Running your program
  , run
  , run_
  )
where

import Drama.Process.Internal
