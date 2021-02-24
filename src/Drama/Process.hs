-- |
-- Module:     Drama.Process
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- TODO

module Drama.Process
  ( Process

    -- * Spawning processes
  , spawn
  , wait

    -- * Sending messages
  , Address
  , send
  , here

    -- * Receiving messages
  , receive
  , tryReceive

    -- * Running your program
  , run

    -- * Not receiving messages
  , NoMsg
  , spawn_
  , run_
  , HasMsg
  )
where

import Drama.Process.Internal
