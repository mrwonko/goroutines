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
    c1 <- newChannel 0
    c2 <- newChannel 0
    go $ do
      writeChannel c1 "hello"
      writeChannel c2 42
      closeChannel c1
      closeChannel c2
    let
      testSelect = select
        [ caseRead c1 $ return . Left
        , caseRead c2 $ return . Right
        ]
    x <- testSelect
    y <- testSelect
    z <- testSelect
    return (x, y, z)
