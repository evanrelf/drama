{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad (replicateM, when)
import Data.Function ((&))
import Drama


main :: IO ()
main = run do
  -- Spawn `fib` process, which starts waiting for requests.
  fibAddr <- spawn fib

  -- Request fibonacci numbers from `fib`. `fib` will spawn three `fibWorker`s
  -- to do all the work in the background, and then send `main` their results
  -- once they finish.
  self <- here
  [200, 400, 600] & mapM_ \n -> send fibAddr (self, n)

  -- Receive results sent back from the `fibWorker`s, and print them to the
  -- console.
  replicateM 3 receive >>= mapM_ \(n, f) ->
    liftIO $ putStrLn ("Fibonacci number " <> show n <> " is " <> show f)

  -- Tell `fib` to stop.
  send fibAddr (self, -1)

  -- Wait for `fib` to stop before exiting.
  wait


-- | Process which immediately spawns and delegates to "worker" processes.
fib :: Process (Address (Int, Integer), Int) ()
fib = do
  (responseAddr, n) <- receive
  when (n >= 0) do
    spawn_ (fibWorker responseAddr n)
    fib


-- | "Worker" process responsible for doing the real, time-consuming work.
fibWorker :: Address (Int, Integer) -> Int -> Process Void ()
fibWorker responseAddr n = send responseAddr (n, fibs !! n)


-- | Infinite list of fibonacci numbers.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
