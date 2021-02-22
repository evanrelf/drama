{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Function ((&))
import Drama


main :: IO ()
main = run_ do
  -- Spawn `fib` process, which starts waiting for requests.
  fibAddr <- spawn fib

  let ns = [200, 400, 600]

  -- Request fibonacci numbers from `fib`. `fib` will spawn three `fibWorker`s
  -- to do all the work in the background, and then send `main` their results
  -- once they finish.
  fs <- mapM (call fibAddr . GetFibNumber) ns

  fs & mapM_ \(n, f) ->
    liftIO $ putStrLn ("Fibonacci number " <> show n <> " is " <> show f)


data FibMsg res where
  GetFibNumber :: Int -> FibMsg (Int, Integer)


-- | Process which immediately spawns and delegates to "worker" processes.
fib :: Process (Envelope FibMsg) ()
fib = forever do
  envelope <- receive
  spawn_ (fibWorker envelope)


-- | "Worker" process responsible for doing the real, time-consuming work. Dies
-- after sending its result to the return address.
fibWorker :: Envelope FibMsg -> Process NoMsg ()
fibWorker = handle \case
  GetFibNumber n -> pure (n, fibs !! n)


-- | Infinite list of fibonacci numbers.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
