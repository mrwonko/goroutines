{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module CSP
  ( CSP
  , go, newChannel, readChannel, writeChannel, closeChannel, orElse
  , runCSP
  , ChannelRef
  ) where

import Control.Monad.ST
import Data.STRef
import Data.Maybe(isJust)

-- Thanks to Alex Biehl for the idea of using continuations

data WriteWaiter s a = forall b. WriteWaiter a (Bool -> CSP s b) (b -> ST s ())

data ReadWaiter s a = forall b. ReadWaiter (Maybe a -> CSP s b) (b -> ST s ())

data ChannelRef s a = ChannelRef (STRef s (Channel s a))
data Channel s a = Channel
  { capacity :: Int
  , closed :: Bool
  -- FIXME: more efficient FIFO
  , elements :: [a]
  -- FIXME: it's pretty unfair to have a LIFO for waiters
  , readWaiters :: [ReadWaiter s a]
  , writeWaiters :: [WriteWaiter s a]
  }

writeBlocks :: Channel s a -> Bool
writeBlocks chan = length (elements chan) == capacity chan

mkChannel :: Int -> Channel s a
mkChannel cap = Channel
    { capacity = cap
    , closed = False
    , elements = []
    , readWaiters = []
    , writeWaiters = []
    }

data CSP s a =
    Return a
  | forall b. Go (CSP s b) (CSP s a)
  | forall b. NewChannel Int (ChannelRef s b -> CSP s a)
  | forall b. ReadChannel (ChannelRef s b) (Maybe b -> CSP s a)
  | forall b. WriteChannel (ChannelRef s b) b (Bool -> CSP s a)
  | forall b. CloseChannel (ChannelRef s b) (Bool -> CSP s a)
  | forall b. OrElse (CSP s b) (CSP s b) (b -> CSP s a) -- choose first to be ready

go csp = Go csp $ return ()
newChannel cap = NewChannel cap return
readChannel :: forall s a. ChannelRef s a -> CSP s (Maybe a)
readChannel chan = ReadChannel chan return
writeChannel chan val = WriteChannel chan val return
closeChannel chan = CloseChannel chan return
a `orElse` b = OrElse a b return

data ReadyCSP s = forall a. ReadyCSP (CSP s a) (a -> ST s ())

data EvalState s a = EvalState
  { esReadyCSPs :: [ReadyCSP s] -- excluding the currently evaluating one
  , esResult :: Maybe a
  }

emptyEvalState :: EvalState s a
emptyEvalState = EvalState
  { esReadyCSPs = []
  , esResult = Nothing
  }

runCSP :: (forall s. CSP s a) -> Maybe a
runCSP g = runST $ do
    stateRef <- newSTRef $ emptyEvalState
    let
      store result = do
        state <- readSTRef stateRef
        writeSTRef stateRef state
          { esResult = Just result
          }
      discard finalResult = return ()

      ready csp finally = do
        state <- readSTRef stateRef
        writeSTRef stateRef state
          { esReadyCSPs = ReadyCSP csp finally : esReadyCSPs state
          }

      -- return: store result, done
      eval (Return a) finally = finally a
      -- go: add to ready CSPs
      eval (Go g cont) finally = do
        state <- readSTRef stateRef
        writeSTRef stateRef state
          { esReadyCSPs = ReadyCSP g discard:esReadyCSPs state
          }
        eval cont finally
      -- newChannel: call continuation with new channel
      eval (NewChannel cap cont) finally = do
        chanRef <- newSTRef $ mkChannel cap
        eval (cont $ ChannelRef chanRef) finally
      -- readChannel: continue if something available, block and continue one of the other CSPs otherwise
      eval (ReadChannel (ChannelRef chanRef) cont) finally = do
        chan <- readSTRef chanRef
        case elements chan of
          -- if there's something in the channel, take that
          x:xs -> do
            -- now let the first write waiter, if any, write (if the channel is closed, there should be none)
            case writeWaiters chan of
              [] -> writeSTRef chanRef chan{ elements = xs }
              WriteWaiter x cont' finally' : ws -> do
                -- remove from waiters and add element
                writeSTRef chanRef chan
                  { elements = xs ++ [x]
                  , writeWaiters = ws
                  }
                -- and add writer to ready
                ready (cont' True) finally'
            eval (cont $ Just x) finally
          [] -> if closed chan
            -- if the channel is closed, continue likewise
            then eval (cont Nothing) finally
            else case writeWaiters chan of
              -- if there is nobody waiting to write, we must now wait to read
              [] -> do
                writeSTRef chanRef chan{ readWaiters = ReadWaiter cont finally : readWaiters chan }
              -- if there's already a writer waiting to write to the channel, let it
              WriteWaiter x cont' finally' : ws -> do
                -- remove writer from writeWaiters
                writeSTRef chanRef chan{ writeWaiters = ws }
                -- put it into ready instead
                ready (cont' True) finally'
                -- and continue executing the reader (we could arguably also execute the writer first)
                eval (cont $ Just x) finally
      -- writeChannel: continue if capacity left in channel, block and continue of the other CSPs otherwise
      eval (WriteChannel (ChannelRef chanRef) x cont) finally = do
        chan <- readSTRef chanRef
        if closed chan
          then eval (cont False) finally
          else  if writeBlocks chan
            then return () -- TODO
            else do
              -- still space in channel, store result and continue
              writeSTRef chanRef chan
                { elements = elements chan ++ [x]
                }
              eval (cont True) finally
      -- closeChannel: set channel to closed, unless it already was, and complete read and write waiters
      eval (CloseChannel (ChannelRef chanRef) cont) finally = return () -- TODO
      -- orElse: TBD
      eval (OrElse g1 g2 cont) finally = return () -- TODO

      evalUntilDone = do
        state <- readSTRef stateRef
        -- stop as soon as there is a result
        if isJust $ esResult state
          then return $ esResult state
          -- otherwise continue while there are more ready CSPs
          else case esReadyCSPs state of
            [] -> return Nothing
            (ReadyCSP csp finally):xs -> do
              writeSTRef stateRef state
                { esReadyCSPs = xs
                }
              eval csp finally
              evalUntilDone

    -- put initial CSP in Ready list
    ready g store
    -- start evaluation loop
    evalUntilDone

instance Functor (CSP s) where
  fmap f (Return a) = Return $ f a
  fmap f (Go g cont) = Go g $ fmap f cont
  fmap f (NewChannel cap cont) = NewChannel cap $ \chan -> fmap f $ cont chan
  fmap f (ReadChannel chan cont) = ReadChannel chan $ \val -> fmap f $ cont val
  fmap f (WriteChannel chan val cont) = WriteChannel chan val $ \success -> fmap f $ cont success
  fmap f (CloseChannel chan cont) = CloseChannel chan $ \success -> fmap f $ cont success
  fmap f (OrElse g1 g2 cont) = OrElse g1 g2 $ \val -> fmap f $ cont val

instance Applicative (CSP s) where
  pure = Return
  (Return f) <*> x = fmap f x
  (Go g cont) <*> x = Go g $ cont <*> x
  (NewChannel cap cont) <*> x = NewChannel cap $ (<*> x) . cont
  (ReadChannel chan cont) <*> x = ReadChannel chan $ (<*> x) . cont
  (WriteChannel chan val cont) <*> x = WriteChannel chan val $ (<*> x) . cont
  (CloseChannel chan cont) <*> x = CloseChannel chan $ (<*> x) . cont
  (OrElse g1 g2 cont) <*> x = OrElse g1 g2 $ (<*> x) . cont 

instance Monad (CSP s) where
  return = pure
  (Return a) >>= f = f a
  (Go g cont) >>= f = Go g $ cont >>= f
  (NewChannel cap cont) >>= f = NewChannel cap $ \chan -> cont chan >>= f
  (ReadChannel chan cont) >>= f = ReadChannel chan $ \val -> cont val >>= f
  (WriteChannel chan val cont) >>= f = WriteChannel chan val $ \success -> cont success >>= f
  (CloseChannel chan cont) >>= f = CloseChannel chan $ \success -> cont success >>= f
  (OrElse g1 g2 cont) >>= f = OrElse g1 g2 $ \val -> cont val >>= f
