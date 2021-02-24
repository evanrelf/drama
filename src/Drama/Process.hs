-- |
-- Module:     Drama.Process
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Lower-level processes, supporting `spawn`, `send`, `receive` and other
-- related operations. Inspired by Elixir and Erlang's processes.

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

    -- * Re-exports
  , liftIO
  )
where

import Control.Monad.IO.Class (liftIO)
import Drama.Process.Internal
