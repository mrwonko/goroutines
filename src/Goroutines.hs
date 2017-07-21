module Goroutines where

-- this will probably need more type parameters for the current internal state? How else can it hold handles to Channels of different types?
data Goroutine a = Goroutine a

instance Functor Goroutine where
  fmap f (Goroutine x) = Goroutine $ f x

instance Applicative Goroutine where
  pure = Goroutine
  (Goroutine f) <*> (Goroutine x) = Goroutine $ f x

instance Monad Goroutine where
  (Goroutine x) >>= f = f x

runGoroutine :: Goroutine a -> a
runGoroutine (Goroutine x) = x

go :: Goroutine a -> Goroutine ()
go (Goroutine x) = Goroutine ()

data ReadChannel a = ReadChannel
data WriteChannel a = WriteChannel

mkChannel :: Int -> Goroutine (ReadChannel a, WriteChannel a)
mkChannel capacity = return (ReadChannel, WriteChannel)

readC :: ReadChannel a -> Goroutine (Maybe a)
readC c = return Nothing

writeC :: WriteChannel a -> a -> Goroutine Bool
writeC c a = return False

-- returns whether the close was successful, i.e. the Channel was not yet closed
close :: WriteChannel a -> Goroutine Bool
close c = return True

-- do we need a close for ReadChannel?