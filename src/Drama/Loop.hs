{-# LANGUAGE LambdaCase #-}

-- |
-- Module:     Drama.Loop
-- Stability:  experimental
-- License:    BSD-3-Clause
-- Copyright:  © 2021 Evan Relf
-- Maintainer: evan@evanrelf.com

module Drama.Loop
  ( loop
  , continue
  , stop

    -- * Re-exports
  , forever
  )
where

import Control.Monad (forever)


-- | Loop indefinitely with state. Use `Control.Monad.forever` for stateless
-- infinite loops.
--
-- ===== __ Example __
--
-- > counter :: Process NoMsg ()
-- > counter = loop (10 :: Int) \count -> do
-- >   liftIO $ print count
-- >   if count > 0
-- >     then continue (count - 1)
-- >     else exit ()
--
-- @since 0.3.0.0
loop
  :: Monad m
  => s
  -- ^ Initial state
  -> (s -> m (Either s a))
  -- ^ Action to perform, returning either a new state to continue looping, or
  -- a final value to stop looping.
  -> m a
loop s0 k =
  k s0 >>= \case
    Left s -> loop s k
    Right x -> pure x


-- | Continue looping with some new state.
--
-- prop> continue s = pure (Left s)
--
-- @since 0.3.0.0
continue :: Monad m => s -> m (Either s a)
continue s = pure (Left s)


-- | Stop looping and return with a final value.
--
-- prop> exit x = pure (Right x)
--
-- @since 0.3.0.0
stop :: Monad m => a -> m (Either s a)
stop x = pure (Right x)
