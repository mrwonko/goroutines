module Main where

import Data.Maybe (isJust, fromJust)
import Goroutines

takeWhileJustM :: Monad m => (a -> Maybe b) -> m a -> m [b]
takeWhileJustM unpack action = fmap reverse $ takeWhileJustM' unpack action []
  where
    takeWhileJustM' unpack action xs = do
      y <- action
      case unpack y of
        (Just x) -> takeWhileJustM' unpack action (x:xs)
        Nothing -> return xs

main :: IO ()
main = do
  putStrLn $ unlines $ runGoroutine $ do
    (r, w) <- mkChannel 0
    go $ do
      writeC w "hello"
      writeC w "world"
      close w
    takeWhileJustM id (readC r)
