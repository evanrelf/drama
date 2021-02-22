-- |
-- Module:     Drama.Server
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- TODO
--

module Drama.Server
  ( Server
  , Envelope (..)

    -- * Sending messages
  , cast
  , call

    -- * Handling messages
  , handle
  )
where

import Drama.Server.Internal
