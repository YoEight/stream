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
type Sink m a = Process m (a -> m ())

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

await :: (a -> Process m o) -> m a -> Process m o
await k r = Await r k Stop Stop 

yield :: o -> Process m o
yield o = Yield o Stop

repeatedly :: Process m o -> Process m o
repeatedly p = go p
  where
    go Stop                 = go p
    go (Yield o n)          = Yield o (go n)
    go (Await r recv fb cl) = Await r (go . recv) fb cl

source :: Foldable f => f o -> Process m o
source = foldMap yield

(<~) :: Process1 a b -> Process m a -> Process m b
Stop <~ src                   = kill src 
Yield o n <~ p                = Yield o (n <~ p)
Await _ _ fb _ <~ Stop        = fb <~ Stop
Await k recv _ _ <~ Yield a n = recv (k a) <~ n
f <~ Await r recv fb cl       = Await r ((f <~) . recv) (f <~ fb) (f <~ cl)

(~>) :: Process m a -> Process1 a b -> Process m b
m ~> ab = ab <~ m

feed :: Process m a -> Sink m a -> Process m ()
feed Stop t                   = kill t
feed src Stop                 = kill src
feed (Yield o n) (Yield f nf) = Await (f o) (const $ feed n nf) Stop Stop
feed (Await r recv fb cl) t   = Await r ((`feed` t) . recv) (feed fb t) (feed cl t)
feed src (Await r recv fb cl) = Await r (feed src . recv) (feed src fb) (feed src cl) 

drain :: Process m o -> Process m v
drain Stop                = Stop
drain (Yield _ n)         = drain n
drain (Await r recv fb c) = Await r (drain . recv) (drain fb) (drain c)

kill :: Process m o -> Process m v
kill Stop          = Stop
kill (Yield _ t)   = kill t
kill (Await _ _ _ c) = drain c 

run :: Monad m => Process m a -> m [a]
run Stop               = return []
run (Yield o n)        = liftM (o:) (run n)
run (Await r recv _ _) = (run . recv) =<< r

run_ :: Monad m => Process m a -> m ()
run_ = liftM (const ()) . run

printer :: Show a => Sink IO a
printer = repeatedly $ await yield (return print)

resPrinter :: Show a => Sink IO a -- simulates the need of a resource
resPrinter = resource (const printer) opening release
  where
    opening = print "Opening"
    release = print "Release" 

resource :: (r -> Process m o) -> m r -> m () -> Process m o
resource k ack cl = Await ack ((<> cleaning) . k) cleaning cleaning 
  where
    cleaning = await (const Stop) cl

test :: Process IO ()
test = feed (source [1..10] ~> auto (+1)) resPrinter
