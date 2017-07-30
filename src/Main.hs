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
  putStrLn $ show $ fmap unlines $ eval $ do
    c <- NewChannel 0 Return
    Go (do
      WriteChannel c "hello" Return
      WriteChannel c "world" Return
      CloseChannel c Return) $ Return ()
    fmap (fmap fromJust) (takeWhileM (isJust) (ReadChannel c Return))
