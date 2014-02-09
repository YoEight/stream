{-# LANGUAGE GADTs #-}
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

import Data.Process.Plan

data Step m o r where
    Yield :: o -> r -> Step m o r
    Await :: m a -> (a -> r) -> r -> Step m o r
    Stop  :: Step m o r

newtype Process m o
    = Process
      { unProcess :: Step m o (Process m o) }

type Channel a m b = Process m (a -> m b)
type Sink m a      = Channel a m ()

repeatedly :: Plan m o u -> Process m o
repeatedly (Plan k) = r
  where
    r = Process $ k
        (const $ unProcess r)
        (\o n -> Yield o (Process n))
        (\rq c fb -> Await rq (Process . c) (Process fb))
        Stop

process :: Plan m o u -> Process m o
process (Plan k) =
    Process $ k
    (const Stop)
    (\o n -> Yield o (Process n))
    (\rq c fb -> Await rq (Process . c) (Process fb))
    Stop

stopped :: Process m o
stopped = Process Stop

eval :: Process m (m a) -> Process m a
eval p =
    Process $
    case unProcess p of
        Stop          -> Stop
        Yield ma n    -> Await ma (\a -> Process $ Yield a (eval n)) stopped
        Await rq c fb -> Await rq (eval . c) (eval fb)

run :: Monad m => Process m a -> m ()
run m =
    case unProcess m of
        Stop         -> return ()
        Yield _ n    -> run n
        Await rq c _ -> run . c =<< rq
