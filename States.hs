module States where
import System.IO
import Control.Applicative
import Data.Char


newtype ST st res = S (st -> (res, st))
newtype StateIO st res = StT (st -> IO (res, st))

app :: ST st res -> st -> (res,st)
app (S trans) s = trans s

appIO :: StateIO st a -> st -> IO (a,st)
appIO (StT trans) s = trans s

lift :: IO a -> StateIO st a
lift m = StT (\s -> do x <- m
                       return (x,s))

stState :: ST st st
stState = S(\st -> (st,st))

stStateIO :: StateIO st st
stStateIO = StT(\st ->do 
 return (st,st))


stUpdate :: st -> ST st()
stUpdate s = S(\_ -> (() , s))

stUpdateIO :: st -> StateIO st()
stUpdateIO s = StT(\_ -> do
 return(() , s))

stRevise :: (st -> st) -> ST st()
stRevise f = do
 st <- stState 
 stUpdate (f st)

stReviseIO :: (st -> st) -> StateIO st()
stReviseIO f = do
 st <- stStateIO 
 stUpdateIO (f st)

instance Functor (ST st) where
 -- fmap  :: (a->b) -> ST st a -> ST st b
 --fmap g sta = S(\s -> ((g x,s') | (x,s') <- app sta s ))
 fmap g sta = S(\s -> let (x, s') = app sta s in (g x, s'))

instance Applicative (ST st) where
 --pure :: a -> ST st a
 pure x = S(\st ->  (x, st))

 --(<*>) :: ST st (sta -> stb) -> ST st sta -> ST st stb
 stf <*> sta = S (\st ->  let (f,st1)= app stf st
                              (x,st2) = app sta st1
                              in (f x, st2) )
instance Monad (ST st) where
 -- return :: a -> ST st a
 -- return = pure
 -- ()>>=) :: ST st a -> (a -> ST st b) -> ST st b
 sta >>= fstb = S (\st -> let (x, st1) = app sta st
                              in app (fstb x) st1)

instance Functor (StateIO st) where
 -- fmap  :: (a->b) -> StateIO st a -> StateIO st b
 fmap g sta = StT (\s -> do 
  (x,s') <- appIO sta s
  return (g x,s'))

instance Applicative (StateIO st) where
 --pure :: a -> ST st a
 pure x = StT(\st -> return(x, st))

 --(<*>) :: ST st (sta -> stb) -> ST st sta -> ST st stb
 stf <*> sta = StT (\st ->  do
 (f,st1) <- appIO stf st
 (x,st2) <- appIO sta st1
 return (f x, st2) )


instance Monad (StateIO st) where
 -- return :: a -> StateIO st a
 -- return = pure

 -- ()>>=) :: StateIO st a -> (a -> StateIO st b) -> StateIO st b
 sta >>= fstb = StT (\st -> do
 (x, st1) <- appIO sta st
 y <- appIO (fstb x) st1
 return y)

type FibST = ST (Integer, Integer)

fib :: FibST Integer
fib = do (a,b) <- stState
         stUpdate (b, b+a)
         return a

fibs :: FibST [Integer]
fibs = do x <- fib
          xs <- fibs
          return (x:xs)

fibonacci :: [Integer]
fibonacci = fst $ app fibs (1,1)


