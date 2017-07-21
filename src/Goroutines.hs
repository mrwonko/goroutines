module Goroutines where

data Goroutine a = Goroutine a -- to be expanded

instance Functor Goroutine where
	fmap f (Goroutine x) = Goroutine $ f x

instance Applicative Goroutine where
	pure = Goroutine
	(Goroutine f) <*> (Goroutine x) = Goroutine $ f x

instance Monad Goroutine where
	(Goroutine x) >>= f = f x

runGoroutine :: Goroutine a -> a
runGoroutine (Goroutine x) = x
