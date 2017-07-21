module Main where

import Goroutines

main :: IO ()
main = do
  putStrLn $ runGoroutine $ do
    return "hello world"
