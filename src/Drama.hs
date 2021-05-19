-- |
-- Module:     Drama
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Actor library for Haskell

module Drama
  ( -- * Lower-level processes
    module Drama.Process

    -- * Higher-level processes
  , module Drama.Server
  )
where

import Drama.Process
import Drama.Server
