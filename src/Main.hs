module Main where

import Data.Maybe (isJust, fromJust)
import Goroutines

takeWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
takeWhileM cond action = fmap reverse $ takeWhileM' cond action []
  where
    takeWhileM' cond action xs = do
      x <- action
      if cond x
        then takeWhileM' cond action (x:xs)
        else return xs

main :: IO ()
main = do
  putStrLn $ unlines $ runGoroutine $ do
    (r, w) <- mkChannel 0
    go $ do
      writeC w "hello"
      writeC w "world"
      close w
    fmap (fmap fromJust) (takeWhileM (isJust) (readC r))
