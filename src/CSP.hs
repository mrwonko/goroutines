{-# LANGUAGE ExistentialQuantification #-}

module CSP
  ( CSP
  , go, newChannel, readChannel, writeChannel, closeChannel, orElse
  , eval
  , ChannelHandle
  ) where

import qualified Data.Map.Lazy as Map -- or Strict?

-- Thanks to Alex Biehl for the idea of using continuations

data WriteWaiter a = forall b. WriteWaiter a (Bool -> CSP b)

data ReadWaiter a = forall b. ReadWaiter (Maybe a -> CSP b)

data ChannelHandle a = ChannelHandle Int
data Channel a = Channel
  { capacity :: Int
  , closed :: Bool
  , elements :: [a]
  , readWaiters :: [ReadWaiter a]
  , writeWaiters :: [WriteWaiter a]
  }
data UntypedChannel = forall a. UntypedChannel (Channel a)

type Channels = Map.Map Int UntypedChannel

writeBlocks :: UntypedChannel -> Bool
writeBlocks (UntypedChannel chan) = length (elements chan) == capacity chan

readBlocks :: UntypedChannel -> Bool
readBlocks (UntypedChannel chan) = null $ elements chan

mkChannel :: Int -> Channel a
mkChannel cap = Channel
    { capacity = cap
    , closed = False
    , elements = []
    , readWaiters = []
    , writeWaiters = []
    }

data CSP a =
    Return a
  | forall b. Go (CSP b) (CSP a)
  | forall b. NewChannel Int (ChannelHandle b -> CSP a)
  | forall b. ReadChannel (ChannelHandle b) (Maybe b -> CSP a)
  | forall b. WriteChannel (ChannelHandle b) b (Bool -> CSP a)
  | forall b. CloseChannel (ChannelHandle b) (Bool -> CSP a)
  | forall b. OrElse (CSP b) (CSP b) (b -> CSP a) -- choose first to be ready

go csp = Go csp $ return ()
newChannel cap = NewChannel cap return
readChannel chan = ReadChannel chan return
writeChannel chan val = WriteChannel chan val return
closeChannel chan = CloseChannel chan return
a `orElse` b = OrElse a b return

data ReadyCSP = forall a. ReadyCSP (CSP a)

data EvalState a = EvalState
  { esResult :: Maybe a
  , esReadyCSPs :: [ReadyCSP] -- excluding the currently evaluating one
  , esChannels :: Channels
  , esChannelCounter :: Int
  }

initialEvalState :: EvalState a
initialEvalState = EvalState
  { esResult = Nothing
  , esReadyCSPs = []
  , esChannels = Map.empty
  , esChannelCounter = 0
  }

esAddChannel :: Int -> EvalState a -> (Channel a, EvalState a)
esAddChannel cap state = let chan = mkChannel cap in (chan, state
  { esChannelCounter = esChannelCounter state + 1
  , esChannels = Map.insert (esChannelCounter state) (UntypedChannel chan) (esChannels state)
  })

esSetChannel :: Int -> (Channel a) -> EvalState b -> EvalState b
esSetChannel id chan state = state
  { esChannels = Map.insert id (UntypedChannel chan) (esChannels state)
  }

-- esGetChannel :: Int -> EvalState a -> Channel b
-- esGetChannel id state = case esChannels state Map.! id of
--   (UntypedChannel chan) -> chan

-- eval :: CS a -> STM a -- STM also has channels, would thus make for a straightforward implementation
eval :: CSP a -> Maybe a
eval g = esResult $ loop g initialEvalState
  where
    loop :: CSP a -> EvalState a -> EvalState a
    -- return: store result, done
    loop (Return a) state = state
      { esResult = Just a
      }
    -- go: add to ready CSPs
    loop (Go g cont) state = loop cont state
      { esReadyCSPs = (ReadyCSP g):esReadyCSPs state
      }
    -- newChannel: call continuation with new channel
    loop (NewChannel cap cont) state = loop (cont $ ChannelHandle $ esChannelCounter state) state
      { esChannelCounter = esChannelCounter state + 1
      } -- TODO: store 
    -- readChannel: continue if something available, block and continue one of the other CSPs otherwise
    loop (ReadChannel chan cont) state = state -- TODO
    -- writeChannel: continue if capacity left in channel, block and continue of the other CSPs otherwise
    loop (WriteChannel chan x cont) state = state -- TODO
    -- closeChannel: set channel to closed, unless it already was
    loop (CloseChannel chan cont) state = state -- TODO
    -- orElse: TBD
    loop (OrElse g1 g2 cont) state = state -- TODO

instance Functor CSP where
  fmap f (Return a) = Return $ f a
  fmap f (Go g cont) = Go g $ fmap f cont
  fmap f (NewChannel cap cont) = NewChannel cap $ \chan -> fmap f $ cont chan
  fmap f (ReadChannel chan cont) = ReadChannel chan $ \val -> fmap f $ cont val
  fmap f (WriteChannel chan val cont) = WriteChannel chan val $ \success -> fmap f $ cont success
  fmap f (CloseChannel chan cont) = CloseChannel chan $ \success -> fmap f $ cont success
  fmap f (OrElse g1 g2 cont) = OrElse g1 g2 $ \val -> fmap f $ cont val

instance Applicative CSP where
  pure = Return
  (Return f) <*> x = fmap f x
  (Go g cont) <*> x = Go g $ cont <*> x
  (NewChannel cap cont) <*> x = NewChannel cap $ (<*> x) . cont
  (ReadChannel chan cont) <*> x = ReadChannel chan $ (<*> x) . cont
  (WriteChannel chan val cont) <*> x = WriteChannel chan val $ (<*> x) . cont
  (CloseChannel chan cont) <*> x = CloseChannel chan $ (<*> x) . cont
  (OrElse g1 g2 cont) <*> x = OrElse g1 g2 $ (<*> x) . cont 

instance Monad CSP where
  return = pure
  (Return a) >>= f = f a
  (Go g cont) >>= f = Go g $ cont >>= f
  (NewChannel cap cont) >>= f = NewChannel cap $ \chan -> cont chan >>= f
  (ReadChannel chan cont) >>= f = ReadChannel chan $ \val -> cont val >>= f
  (WriteChannel chan val cont) >>= f = WriteChannel chan val $ \success -> cont success >>= f
  (CloseChannel chan cont) >>= f = CloseChannel chan $ \success -> cont success >>= f
  (OrElse g1 g2 cont) >>= f = OrElse g1 g2 $ \val -> cont val >>= f
