{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Data.Process where

import Control.Applicative
import Control.Monad
import Data.Semigroup
import Data.Foldable (Foldable, foldMap)
import Data.List.NonEmpty hiding (head, tail)
import Data.Monoid (Monoid (..))

newtype Process m o = Process
    { unProcess ::
           forall r.
           (NonEmpty o -> Process m o -> r) ->
           (forall a. m a -> (a -> Process m o) -> Process m o -> r) ->
           r ->
           r }

instance Functor (Process m) where
    fmap f (Process k) = Process $ \onYield onAwait onHalt ->
        k (\xs next -> onYield (fmap f xs) (fmap f next))
        (\req recv fb -> onAwait req (fmap f . recv) (fmap f fb))
        onHalt

instance Applicative (Process m) where
    pure  = return
    (<*>) = ap

instance Monad (Process m) where
    return a = Process $ \onYield _ _ -> onYield (nel a) halt

    Process k >>= f = Process $ \onYield onAwait onHalt ->
        let yielding (x :| xs) next =
                let (Process action) = append (f x) (rest >>= f)
                    rest =
                        if null xs
                        then next
                        else yieldAllWith (head xs :| tail xs) next in
                action onYield onAwait onHalt
            awaiting req recv fb =
                let (Process action) =
                        awaitWith req ((f =<<) . recv) (fb >>= f) in
                action onYield onAwait onHalt in
        k yielding awaiting onHalt

collectProcess :: Monad m => Process m o -> m [o]
collectProcess (Process k) =
    let onYield xs next     = liftM ((toList xs) ++) (collectProcess next)
        onAwait req recv fb = collectProcess . recv =<< req
        onHalt              = return [] in
    k onYield onAwait onHalt

halt :: Process m o
halt = Process $ \_ _ h -> h

await :: m a -> (a -> Process m o) -> Process m o
await req k = awaitWith req k halt

awaitWith :: m a -> (a -> Process m o) -> Process m o -> Process m o
awaitWith req k fb = Process $ \_ onAwait _ -> onAwait req k fb

yield :: o -> Process m o
yield o = yieldAllWith (nel o) halt

yieldAll :: NonEmpty o -> Process m o
yieldAll xs = yieldAllWith xs halt

yieldAllWith :: NonEmpty o -> Process m o -> Process m o
yieldAllWith xs next = Process $ \onYield _ _ -> onYield xs next

nel :: a -> NonEmpty a
nel a = a :| []

append :: Process m o -> Process m o -> Process m o
append (Process kl) p2@(Process kr) = Process $ \onYield onAwait onHalt ->
    kl (\xs next -> onYield xs (append next p2))
    (\r k fb -> onAwait r (\a -> append (k a) p2) (append fb p2))
    (kr onYield onAwait onHalt)

printIt :: Show a => a -> Process IO ()
printIt a = await (print a) yield

test :: Show a => [a] -> Process IO ()
test = mapM_ printIt
