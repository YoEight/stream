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
import Control.Exception
import Control.Monad

newtype Plan f o a
    = Plan
      { unPlan :: forall r.
        (a -> r) ->                                   -- done
        (o -> r -> r) ->                              -- yield
        (forall a. f a -> (a -> r) -> r -> r -> r) -> -- await
        (Maybe SomeException -> r) ->                 -- halt
        r
      }

instance Functor (Plan f o) where
    fmap f (Plan k) = Plan $ \kd -> k (kd . f)

instance Applicative (Plan f o) where
    pure  = return
    (<*>) = ap

instance Monad (Plan f o) where
    return a = Plan $ \kd _ _ _ -> kd a

    Plan k >>= f = Plan $ \kd ky ka kr ->
        let onDone a = unPlan (f a) kd ky ka kr in
        k onDone ky ka kr

await :: f a -> Plan f o a
await fa = Plan $ \kd _ ka kr ->
    let stp = kr Nothing in
    ka fa kd stp stp

yield :: o -> Plan f o ()
yield o = Plan $ \kd ky _ kr -> ky o (kr Nothing)

stop :: Plan f o a
stop = Plan $ \_ _ _ kr -> kr Nothing

failed :: SomeException -> Plan f o a
failed e = Plan $ \_ _ _ kr -> kr $ Just e
