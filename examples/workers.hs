{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad (replicateM, when)
import Data.Function ((&))
import Drama


main :: IO ()
main = run do
  -- Spawn `fib` process, which starts waiting for requests.
  fibAddress <- spawn fib

  -- Request fibonacci numbers from `fib`. `fib` will spawn three `fibWorker`s
  -- to do all the work in the background, and then send `main` their results
  -- once they finish.
  myAddress <- here
  [200, 400, 600] & mapM_ \n -> send fibAddress (myAddress, n)

  -- Receive results sent back from the `fibWorker`s, and print them to the
  -- console.
  replicateM 3 receive >>= mapM_ \(n, f) ->
    liftIO $ putStrLn ("Fibonacci number " <> show n <> " is " <> show f)

  -- Tell `fib` to stop.
  send fibAddress (myAddress, -1)

  -- Wait for `fib` to stop before exiting.
  wait


-- | Process which immediately spawns and delegates to "worker" processes.
fib :: Process (Address (Int, Integer), Int) ()
fib = do
  (responseAddress, n) <- receive
  when (n >= 0) do
    spawn_ (fibWorker responseAddress n)
    fib


-- | "Worker" process responsible for doing the real, time-consuming work.
fibWorker :: Address (Int, Integer) -> Int -> Process Void ()
fibWorker responseAddress n = send responseAddress (n, fibs !! n)


-- | Infinite list of fibonacci numbers.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
