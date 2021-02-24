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

    -- * Re-exports
  , MonadIO (..)
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Drama.Process.Internal
