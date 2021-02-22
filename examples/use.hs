{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Monad (when)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Drama
import System.Exit (exitSuccess)


main :: IO ()
main = run_ do
  bottles <- useCounter 99

  forever do
    n <- getCount bottles

    when (n <= 0) do
      liftIO $ exitSuccess

    liftIO $ putStrLn (show n <> " bottle(s) of beer on the wall, " <> show n <> " bottle(s) of beer")

    bottles -= 1
    n' <- getCount bottles

    liftIO $ putStrLn ("Take one down and pass it around, " <> show n' <> " bottle(s) of beer")


--------------------------------------------------------------------------------


data CounterMsg res where
  Increment :: Int -> CounterMsg ()
  Decrement :: Int -> CounterMsg ()
  GetCount :: CounterMsg Int


counter :: Int -> Server CounterMsg ()
counter count0 = do
  UseState{get, modify} <- useState count0

  forever $ handle \case
    Increment n -> modify (+ n)
    Decrement n -> modify (+ negate n)
    GetCount -> get


data UseCounter = UseCounter
  { increment :: forall msg. Int -> Process msg ()
  , decrement :: forall msg. Int -> Process msg ()
  , (+=) :: forall msg. Int -> Process msg ()
  , (-=) :: forall msg. Int -> Process msg ()
  , getCount :: forall msg. Process msg Int
  }


useCounter :: Int -> Process msg UseCounter
useCounter count0 = do
  counterAddr <- spawn (counter count0)

  pure UseCounter
    { increment = \n -> cast counterAddr (Increment n)
    , decrement = \n -> cast counterAddr (Decrement n)
    , (+=) = \n -> cast counterAddr (Increment n)
    , (-=) = \n -> cast counterAddr (Decrement n)
    , getCount = call counterAddr GetCount
    }


--------------------------------------------------------------------------------


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


data UseState s = UseState
  { get :: forall msg. HasMsg s => Process msg s
  , gets :: forall a msg. HasMsg a => (s -> a) -> Process msg a
  , put :: forall msg. s -> Process msg ()
  , modify :: forall msg. (s -> s) -> Process msg ()
  }


useState :: s -> Process msg (UseState s)
useState s0 = do
  stateAddr <- spawn (state s0)

  pure UseState
    { get = call stateAddr GetState
    , gets = \f -> call stateAddr (GetsState f)
    , put = \s -> cast stateAddr (PutState s)
    , modify = \f -> cast stateAddr (ModifyState f)
    }
