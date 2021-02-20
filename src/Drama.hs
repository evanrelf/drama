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
  )
where

import Drama.Internal
