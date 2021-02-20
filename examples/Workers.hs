{-# LANGUAGE BlockArguments #-}

module Workers (main) where

import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Drama


main :: IO ()
main = run do
  myAddress <- here

  fibAddress <- spawn fib

  [200, 400, 600] & mapM_ \n -> send fibAddress (myAddress, n)

  replicateM 3 receive >>= mapM_ \(n, f) ->
    liftIO $ putStrLn ("Fibonacci number " <> show n <> " is " <> show f)

  send fibAddress (myAddress, -1)

  wait


fib :: Actor (Address (Int, Integer), Int) ()
fib = do
  (responseAddress, n) <- receive
  when (n >= 0) do
    _ <- spawn (fibWorker responseAddress n)
    fib


fibWorker :: Address (Int, Integer) -> Int -> Actor () ()
fibWorker responseAddress n = send responseAddress (n, fibs !! n)


fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
