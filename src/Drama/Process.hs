-- |
-- Module:     Drama.Process
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- TODO

module Drama.Process
  ( -- * Spawning processes
    spawn
  , spawn_
  , wait

    -- * Sending and receiving messages
  , send
  , receive
  , tryReceive
  , here

    -- * Running your program
  , run
  , run_

    -- * Types
  , Process
  , Address
  , HasMsg
  , NoMsg
  )
where

import Drama.Process.Internal
