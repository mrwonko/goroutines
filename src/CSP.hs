{-# LANGUAGE ExistentialQuantification #-}

module CSP
  ( CSP
  , go, newChannel, readChannel, writeChannel, closeChannel, orElse
  , eval
  , ChannelHandle
  ) where

import Control.Monad.ST
import Data.STRef

-- Thanks to Alex Biehl for the idea of using continuations

data WriteWaiter a = forall b. WriteWaiter a (Bool -> CSP b)

data ReadWaiter a = forall b. ReadWaiter (Maybe a -> CSP b)

data ChannelHandle a = forall s. ChannelHandle (STRef s (Channel a))
data Channel a = Channel
  { capacity :: Int
  , closed :: Bool
  , elements :: [a]
  , readWaiters :: [ReadWaiter a]
  , writeWaiters :: [WriteWaiter a]
  }

writeBlocks :: Channel a -> Bool
writeBlocks chan = length (elements chan) == capacity chan

readBlocks :: Channel a -> Bool
readBlocks chan = null $ elements chan

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

data ReadyCSP s = forall a. ReadyCSP (CSP a)

data EvalState s a = EvalState
  { esReadyCSPs :: [ReadyCSP s] -- excluding the currently evaluating one
  , esChannelCounter :: Int
  }

initialEvalState :: EvalState s a
initialEvalState = EvalState
  { esReadyCSPs = []
  , esChannelCounter = 0
  }

eval :: CSP a -> Maybe a
eval g = runST $ do
    stateRef <- newSTRef initialEvalState
    let
      -- return: store result, done
      eval' (Return a) = return $ Just a
      -- go: add to ready CSPs
      eval' (Go g cont) = do
        state <- readSTRef stateRef
        writeSTRef stateRef state
          { esReadyCSPs = (ReadyCSP g):esReadyCSPs state
          }
        eval' cont
      -- newChannel: call continuation with new channel
      eval' (NewChannel cap cont) = do
        chan <- newSTRef $ mkChannel cap
        eval' $ cont $ ChannelHandle chan
      -- readChannel: continue if something available, block and continue one of the other CSPs otherwise
      eval' (ReadChannel chan cont) = undefined -- TODO
      -- writeChannel: continue if capacity left in channel, block and continue of the other CSPs otherwise
      eval' (WriteChannel chan x cont) = undefined -- TODO
      -- closeChannel: set channel to closed, unless it already was
      eval' (CloseChannel chan cont) = undefined -- TODO
      -- orElse: TBD
      eval' (OrElse g1 g2 cont) = undefined -- TODO
    eval' g

instance Functor (CSP) where
  fmap f (Return a) = Return $ f a
  fmap f (Go g cont) = Go g $ fmap f cont
  fmap f (NewChannel cap cont) = NewChannel cap $ \chan -> fmap f $ cont chan
  fmap f (ReadChannel chan cont) = ReadChannel chan $ \val -> fmap f $ cont val
  fmap f (WriteChannel chan val cont) = WriteChannel chan val $ \success -> fmap f $ cont success
  fmap f (CloseChannel chan cont) = CloseChannel chan $ \success -> fmap f $ cont success
  fmap f (OrElse g1 g2 cont) = OrElse g1 g2 $ \val -> fmap f $ cont val

instance Applicative (CSP) where
  pure = Return
  (Return f) <*> x = fmap f x
  (Go g cont) <*> x = Go g $ cont <*> x
  (NewChannel cap cont) <*> x = NewChannel cap $ (<*> x) . cont
  (ReadChannel chan cont) <*> x = ReadChannel chan $ (<*> x) . cont
  (WriteChannel chan val cont) <*> x = WriteChannel chan val $ (<*> x) . cont
  (CloseChannel chan cont) <*> x = CloseChannel chan $ (<*> x) . cont
  (OrElse g1 g2 cont) <*> x = OrElse g1 g2 $ (<*> x) . cont 

instance Monad (CSP) where
  return = pure
  (Return a) >>= f = f a
  (Go g cont) >>= f = Go g $ cont >>= f
  (NewChannel cap cont) >>= f = NewChannel cap $ \chan -> cont chan >>= f
  (ReadChannel chan cont) >>= f = ReadChannel chan $ \val -> cont val >>= f
  (WriteChannel chan val cont) >>= f = WriteChannel chan val $ \success -> cont success >>= f
  (CloseChannel chan cont) >>= f = CloseChannel chan $ \success -> cont success >>= f
  (OrElse g1 g2 cont) >>= f = OrElse g1 g2 $ \val -> cont val >>= f
