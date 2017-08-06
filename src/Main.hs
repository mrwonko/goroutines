module Main where

import Data.Maybe (isJust, fromJust)
import CSP

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
  putStrLn $ show $ runCSP $ do
    c <- newChannel 0
    go $ do
      writeChannel c "hello"
      writeChannel c "world"
      closeChannel c
    fmap (fmap fromJust) (takeWhileM (isJust) (readChannel c))
