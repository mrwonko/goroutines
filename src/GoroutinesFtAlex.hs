module GoroutinesFtAlex where


Map ChannelId [a]


-- eval :: Goroutine a -> STM a -- STM also has channels, would be a nigh trivial implementation
eval :: Goroutine a -> Maybe a
eval g = loop g 
  where 
    loop (Return a) = return a
    loop (ReadChannel chan k) = undefined
    loop (WriteChannel chan x k) = undefined
    loop (OrElse g1 g2 k) = undefined

data Goroutine a = 
    Return a
  | forall b. NewChannel Int (ReadChannel b  -> WriteChannel b -> Goroutine a)
  | forall b. ReadChannel (ReadChannel b) (b -> Goroutine a)
  | forall b. WriteChannel (WriteChannel b) b (Bool -> Goroutine a)
  | forall b. OrElse (Goroutine b) (Goroutine b) (b -> Goroutine a) -- choose first to be ready

orElse :: Goroutine a -> Goroutine a -> Goroutine a

fmap Left (readC channel1) `orElse` fmap Right (readC channel2)
