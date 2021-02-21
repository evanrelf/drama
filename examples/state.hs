{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Drama
import Drama.Server.Internal (Server (..))

main :: IO ()
main = do
  undefined


data CounterMsg res where
  Increment :: Int -> CounterMsg ()
  GetCount :: CounterMsg Int


counter :: Server CounterMsg ()
counter = do
  stateAddress <- start $ state (0 :: Int)

  forever $ handle \case
    Increment n ->
      cast stateAddress (ModifyState (+ n))

    GetCount ->
      call stateAddress GetState


data StateMsg s res where
  GetState :: StateMsg s s
  GetsState :: (s -> a) -> StateMsg s a
  PutState :: s -> StateMsg s ()
  ModifyState :: (s -> s) -> StateMsg s ()


state :: s -> Server (StateMsg s) ()
state s0 = do
  stateIORef <- liftIO $ newIORef s0

  forever $ handle $ liftIO . \case
    GetState ->
      readIORef stateIORef

    GetsState f -> do
      s <- readIORef stateIORef
      pure (f s)

    PutState s ->
      writeIORef stateIORef s

    ModifyState f ->
      modifyIORef stateIORef f
