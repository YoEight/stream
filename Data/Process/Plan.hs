{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Process.Plan
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
----------------------------------------------------------------------------
module Data.Process.Plan where

import Control.Applicative
import Control.Monad

newtype Plan f o u
    = Plan
      { unPlan ::
             forall r.
             (u -> r) ->                              -- done
             (o -> r -> r) ->                         -- emit
             (forall a. f a -> (a -> r) -> r -> r) -> -- await
             r ->                                     -- halt
             r
      }

instance Functor (Plan f o) where
    fmap f (Plan k) = Plan $ \kp -> k (kp . f)

instance Applicative (Plan f o) where
    pure = return
    (<*>) = ap

instance Monad (Plan f o) where
    return a = Plan $ \kp _ _ _ -> kp a

    Plan k >>= f = Plan $ \kp ke ka kr ->
        k (\a -> unPlan (f a) kp ke ka kr) ke ka kr

-- | Use fallback only if Await req has been requested
onAwaitFb :: (a -> Plan f o u)
          -> f a
          -> Plan f o u
          -> Plan f o u
onAwaitFb k rq fb = Plan $ \kp ke ka kr ->
    let go a = unPlan (k a) kp ke ka (unPlan fb kp ke ka kr) in
    ka rq go kr

onAwait :: (a -> Plan f o u) -> f a -> Plan f o u
onAwait k rq = Plan $ \kp ke ka kr ->
    let go a = unPlan (k a) kp ke ka kr in
    ka rq go kr

await :: f a -> Plan f o a
await fa = Plan $ \kp _ ka kr -> ka fa kp kr

yield :: o -> Plan f o ()
yield o = Plan $ \kp ke _ _ -> ke o (kp ())

stop :: Plan f o a
stop = Plan $ \_ _ _ kr -> kr
