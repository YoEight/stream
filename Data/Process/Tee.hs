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
tee p1 p2 p3 =
    Process $
    case unProcess p1 of
        Stop      -> Stop
        Yield c n -> Yield c (tee n p2 p3)
        Await L k fb ->
            case unProcess p2 of
                Stop            -> unProcess (tee fb stopped p3)
                Yield a p2n     -> unProcess (tee (k a) p2n p3)
                Await rq ca pfa -> Await rq (\r -> tee p1 (ca r) p3)
                                   (tee p1 pfa p3)
        Await R k fb ->
            case unProcess p3 of
                Stop            -> unProcess (tee fb p2 stopped)
                Yield b p3n     -> unProcess (tee (k b) p2 p3n)
                Await rq cb pfb -> Await rq (\r -> tee p1 p2 (cb r))
                                   (tee p1 p2 pfb)
