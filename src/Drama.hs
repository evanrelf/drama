-- |
-- Module:     Drama
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com
--
-- Simple actor library for Haskell
--
-- ==== __Example__
--
-- Fizz buzz, using three actors: @main@, @logger@, and @fizzBuzz@:
--
-- > {-# LANGUAGE BlockArguments #-}
-- > {-# LANGUAGE MultiWayIf #-}
-- > {-# LANGUAGE NumericUnderscores #-}
-- >
-- > module Main (main) where
-- >
-- > import Control.Concurrent (threadDelay)
-- > import Control.Monad (forever)
-- > import Control.Monad.IO.Class (MonadIO (..))
-- > import Drama
-- > import Prelude hiding (log)
-- >
-- > main :: IO ()
-- > main = run do
-- >   loggerAddress <- spawn logger
-- >   _ <- spawn (fizzBuzz loggerAddress)
-- >   wait
-- >
-- > logger :: Actor String ()
-- > logger = forever do
-- >   string <- receive
-- >   liftIO $ putStrLn string
-- >
-- > fizzBuzz :: Address String -> Actor () ()
-- > fizzBuzz loggerAddress = do
-- >   let log = send loggerAddress
-- >
-- >   loop (0 :: Int) \n -> do
-- >     if | n `mod` 15 == 0 -> log "FizzBuzz"
-- >        | n `mod`  3 == 0 -> log "Fizz"
-- >        | n `mod`  5 == 0 -> log "Buzz"
-- >        | otherwise       -> log (show n)
-- >
-- >     liftIO $ threadDelay 500_000
-- >
-- >     continue (n + 1)
--
-- Output:
--
-- > λ> main
-- > 1
-- > 2
-- > Fizz
-- > 4
-- > Buzz
-- > Fizz
-- > 7
-- > 8
-- > Fizz
-- > Buzz
-- > 11
-- > Fizz
-- > 13
-- > 14
-- > FizzBuzz
-- > ...

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
