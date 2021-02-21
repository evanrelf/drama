{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Drama


main :: IO ()
main = run_ do
  counterAddr <- spawn counter
  cast counterAddr (Increment 42)
  count <- call counterAddr GetCount
  liftIO $ print count


data CounterMsg res where
  Increment :: Int -> CounterMsg ()
  GetCount :: CounterMsg Int


counter :: Server CounterMsg ()
counter = do
  stateAddr <- spawn $ state (0 :: Int)

  forever $ handle \case
    Increment n ->
      cast stateAddr (ModifyState (+ n))

    GetCount ->
      call stateAddr GetState


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
