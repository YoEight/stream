{-# LANGUAGE ExistentialQuantification #-}
module Data.Process where

import Control.Applicative
import Control.Monad
import Data.Semigroup
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid (..))

infixr 9 <~
infixl 9 ~>

data Process m o = Yield o (Process m o)
                 | forall a. Await (m a) (a -> Process m o) (Process m o) (Process m o)
                 | Stop

type Process1 a b = Process ((->) a) b

class Automaton k where
  auto :: k a b -> Process1 a b

instance Automaton (->) where
  auto f = repeatedly $ do
             a <- await1
             yield (f a)

instance Functor (Process m) where
  fmap = liftM

instance Applicative (Process m) where
  pure  = return
  (<*>) = ap

instance Monad (Process m) where
  return = yield
  Stop >>= _               = Stop
  Yield o n >>= f          = f o <> (n >>= f)
  Await r recv fb cl >>= f = Await r ((f =<<) . recv) (fb >>= f) (cl >>= f) 

instance Semigroup (Process m o) where
  Stop <> p               = p
  Yield o n <> p          = Yield o (n <> p)
  Await r recv fb cl <> p = Await r ((<> p) . recv) (fb <> p) (cl <> p)

instance Monoid (Process m o) where
  mempty  = Stop
  mappend = (<>)

await1 :: Process1 a a
await1 = Await id (\a -> Yield a Stop) Stop Stop

yield :: o -> Process m o
yield o = Yield o Stop

repeatedly :: Process m o -> Process m o
repeatedly p = r where r = p <> r

source :: Foldable f => f o -> Process m o
source = foldMap yield

(<~) :: Process1 a b -> Process m a -> Process m b
Stop <~ _                     = Stop -- handle finalization of left process
Yield o n <~ p                = Yield o (n <~ p)
Await _ _ fb _ <~ Stop        = fb <~ Stop
Await k recv _ _ <~ Yield a n = recv (k a) <~ n
f <~ Await r recv fb cl       = Await r ((f <~) . recv) (f <~ fb) (f <~ cl) 

(~>) :: Process m a -> Process1 a b -> Process m b
m ~> ab = ab <~ m
