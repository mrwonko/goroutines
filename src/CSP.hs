{-# LANGUAGE ExistentialQuantification #-}

module CSP
  ( CSP
  , go, newChannel, readChannel, writeChannel, closeChannel, orElse
  , eval
  , Channel
  ) where

-- Thanks to Alex Biehl for the idea of using continuations

type ChannelID = Int
data Channel a = Channel
  { id :: ChannelID
  , capacity :: Int
  , closed :: Bool
  , elements :: [a]
  }

--Map ChannelId [a]

data CSP a =
    Return a
  | forall b. Go (CSP b) (CSP a)
  | forall b. NewChannel Int (Channel b -> CSP a)
  | forall b. ReadChannel (Channel b) (Maybe b -> CSP a)
  | forall b. WriteChannel (Channel b) b (Bool -> CSP a)
  | forall b. CloseChannel (Channel b) (Bool -> CSP a)
  | forall b. OrElse (CSP b) (CSP b) (b -> CSP a) -- choose first to be ready
  -- | forall b. AndThen (CSP b) (b -> CSP a) -- This would make implementing fmap, <*> and >>= trivial

go csp = Go csp $ return ()
newChannel cap = NewChannel cap return
readChannel chan = ReadChannel chan return
writeChannel chan val = WriteChannel chan val return
closeChannel chan = CloseChannel chan return
a `orElse` b = OrElse a b return

-- eval :: CS a -> STM a -- STM also has channels, would thus make for a straightforward implementation
eval :: CSP a -> Maybe a
eval g = loop g
  where 
    loop (Return a) = return a
    loop (Go g cont) = Nothing
    loop (NewChannel cap cont) = Nothing
    loop (ReadChannel chan cont) = Nothing
    loop (WriteChannel chan x cont) = Nothing
    loop (CloseChannel chan cont) = Nothing
    -- loop (AndThen g cont) = undefined
    loop (OrElse g1 g2 cont) = Nothing

instance Functor CSP where
  --fmap f x = x `AndThen` (Return . f)
  fmap f (Return a) = Return $ f a
  fmap f (Go g cont) = Go g $ fmap f cont
  fmap f (NewChannel cap cont) = NewChannel cap $ \chan -> fmap f $ cont chan
  fmap f (ReadChannel chan cont) = ReadChannel chan $ \val -> fmap f $ cont val
  fmap f (WriteChannel chan val cont) = WriteChannel chan val $ \success -> fmap f $ cont success
  fmap f (CloseChannel chan cont) = CloseChannel chan $ \success -> fmap f $ cont success
  -- fmap f (AndThen g1 cont) = AndThen g1 $ \val -> fmap f $ cont val
  fmap f (OrElse g1 g2 cont) = OrElse g1 g2 $ \val -> fmap f $ cont val

instance Applicative CSP where
  pure = Return
  --f <*> x = f `AndThen` \f -> fmap f x
  (Return f) <*> x = fmap f x
  (Go g cont) <*> x = Go g $ cont <*> x
  (NewChannel cap cont) <*> x = NewChannel cap $ (<*> x) . cont
  (ReadChannel chan cont) <*> x = ReadChannel chan $ (<*> x) . cont
  (WriteChannel chan val cont) <*> x = WriteChannel chan val $ (<*> x) . cont
  (CloseChannel chan cont) <*> x = CloseChannel chan $ (<*> x) . cont
  (OrElse g1 g2 cont) <*> x = OrElse g1 g2 $ (<*> x) . cont 

instance Monad CSP where
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  (Return a) >>= f = f a
  (Go g cont) >>= f = Go g $ cont >>= f
  (NewChannel cap cont) >>= f = NewChannel cap $ \chan -> cont chan >>= f
  (ReadChannel chan cont) >>= f = ReadChannel chan $ \val -> cont val >>= f
  (WriteChannel chan val cont) >>= f = WriteChannel chan val $ \success -> cont success >>= f
  (CloseChannel chan cont) >>= f = CloseChannel chan $ \success -> cont success >>= f
  (OrElse g1 g2 cont) >>= f = OrElse g1 g2 $ \val -> cont val >>= f

--fmap Left (readC channel1) `orElse` fmap Right (readC channel2)
