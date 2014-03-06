{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Process.Tee
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
----------------------------------------------------------------------------
module Data.Process.Tee where

import Data.Process.Plan
import Data.Process.Type

data T a b c where
    L :: T a b a
    R :: T a b b

type Tee a b c = Process (T a b) c

awaitL :: Plan (T a b) o a
awaitL = await L

awaitR :: Plan (T a b) o b
awaitR = await R

zipWith :: (a -> b -> c) -> Tee a b c
zipWith f = repeatedly $ do
    a <- awaitL
    b <- awaitR
    yield (f a b)

tee :: Tee a b c -> Process m a -> Process m b -> Process m c
tee p1 p2 p3 = Process $
      case unProcess p1 of
          Stop e          -> unProcess
                             (kill p2 `onComplete`
                             (kill p3 `onComplete` stoppedMaybe e))
          Yield c n       -> Yield c (tee n p2 p3)
          Await L k fb cl ->
              case unProcess p2 of
                  Stop e              -> unProcess (tee fb (stoppedMaybe e) p3)
                  Yield a n2          -> unProcess (tee (k a) n2 p3)
                  Await rq k2 fb2 cl2 -> Await rq (\r -> tee p1 (k2 r) p3)
                                         (tee p1 fb2 p3) (tee p1 cl2 p3)
          Await R k fb cl ->
              case unProcess p3 of
                  Stop e              -> unProcess (tee fb p2 (stoppedMaybe e))
                  Yield b n3          -> unProcess (tee (k b) p2 n3)
                  Await rq k3 fb3 cl3 -> Await rq (\r -> tee p1 p2 (k3 r))
                                         (tee p1 p2 fb3) (tee p1 p2 cl3)
