{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Process.Type
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
----------------------------------------------------------------------------
module Data.Process.Type where

import Control.Exception
import Data.Typeable

import Data.Process.Plan

data CauseBy = CauseBy !SomeException !SomeException
               deriving (Show, Typeable)

instance Exception CauseBy

data Step m o r where
    Yield :: o -> r -> Step m o r
    Await :: m a -> (a -> r) -> r -> r -> Step m o r
    Stop  :: Maybe SomeException -> Step m o r

newtype Process m o = Process { unProcess :: Step m o (Process m o) }

type Channel a m b = Process m (a -> m b)
type Sink m a      = Channel a m ()


repeatedly :: Plan m o u -> Process m o
repeatedly (Plan k) = r
  where
    r = Process $ k
        (const $ unProcess r)
        (\o n -> Yield o (Process n))
        (\rq c fb cl -> Await rq (Process . c) (Process fb) (Process cl))
        (\e -> Stop e)

process :: Plan m o u -> Process m o
process (Plan k) =
    Process $ k
    (const $ Stop Nothing)
    (\o n -> Yield o (Process n))
    (\rq c fb cl -> Await rq (Process . c) (Process fb) (Process cl))
    (\e -> Stop e)

before :: Process m o -> Plan m o u -> Process m o
before (Process p) (Plan k) =
    Process $ k
    (const p)
    (\o n -> Yield o (Process n))
    (\rq c fb cl -> Await rq (Process . c) (Process fb) (Process cl))
    (\e -> Stop e)

append :: Process m o -> Process m o -> Process m o
append p1 p2@(Process k2) = Process $
    case unProcess p1 of
        r@(Stop e)        -> maybe k2 (const r) e
        Yield o n         -> Yield o (n `append` p2)
        Await rq k fb cl  -> Await rq ((flip append $ p2) . k)
                             (fb `append` p2) (cl `append` p2)

awaitingWith :: m a
         -> (a -> Process m o)
         -> Process m o
         -> Process m o
         -> Process m o
awaitingWith rq k fb cl = Process $ Await rq k fb cl

awaiting :: m a -> (a -> Process m o) -> Process m o
awaiting rq k = awaitingWith rq k stopped stopped

yieldingWith :: o -> Process m o -> Process m o
yieldingWith o n = Process $ Yield o n

yielding :: o -> Process m o
yielding o = yieldingWith o stopped

stopped :: Process m o
stopped = Process $ Stop Nothing

stoppedMaybe :: Maybe SomeException -> Process m o
stoppedMaybe e = Process $ Stop e

eval :: Process m (m a) -> Process m a
eval p =
    Process $
    case unProcess p of
        Stop e           -> Stop e
        Yield ma n       -> Await ma (liftEval n) stopped stopped
        Await rq c fb cl -> Await rq (eval . c) (eval fb) (eval cl)
  where
    liftEval n a = Process $ Yield a (eval n)

fit :: (forall a. k a -> m a) -> Process k o -> Process m o
fit f p =
    Process $
    case unProcess p of
        Stop e           -> Stop e
        Yield o n        -> Yield o (fit f n)
        Await rq c fb cl -> Await (f rq) (fit f . c) (fit f fb) (fit f cl)

causedBy :: Process m o -> Maybe SomeException -> Process m o
causedBy p Nothing = p
causedBy p (Just e) = loop e p
  where
    loop e cur =
        Process $
        case unProcess cur of
            Yield o n        -> Yield o (loop e n)
            Await rq k fb cl -> Await rq (loop e . k) (loop e fb) (loop e cl)
            Stop o_m         -> Stop $ Just $
                                case o_m of
                                    Nothing -> SomeException e
                                    Just e2 -> SomeException $ CauseBy e2 e

onComplete :: Process m o -> Process m o -> Process m o
onComplete p1 p2 =
    Process $
    case unProcess p1 of
        r@(Stop e)       -> maybe r (const $ unProcess $ causedBy p2 e) e
        Yield o n        -> Yield o (n `onComplete` p2)
        Await rq k fb cl -> Await rq ((flip onComplete $ p2) . k)
                            (fb `onComplete` p2) (cl `onComplete` p2)

drain :: Process m o -> Process m a
drain p =
    Process $
    case unProcess p of
        Stop e           -> Stop e
        Yield _ n        -> unProcess $ drain n
        Await rq k fb cl -> Await rq (drain . k) (drain fb) (drain cl)

kill :: Process m o -> Process m a
kill p =
    Process $
    case unProcess p of
        Stop e         -> Stop e
        Yield _ n      -> unProcess $ kill n
        Await _ _ _ cl -> unProcess $ drain cl
