{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Process.Process1
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
----------------------------------------------------------------------------
module Data.Process.Process1 where

import GHC.Exts

import Data.Process.Plan
import Data.Process.Type
import Data.Process.Tee

infixr 9 <~
infixl 9 ~>

data Is a b where
    Is :: Is a a

type Process1 a b = Process (Is a) b

await1 :: Plan (Is a) o a
await1 = await Is

(<~) :: Process1 a b -> Process m a -> Process m b
(<~) = pipe

(~>) :: Process m a -> Process1 a b -> Process m b
p1 ~> p2 = p2 <~ p1

pipe :: Process1 a b -> Process m a -> Process m b
pipe p1 p2 = tee (fit (\Is -> L) p1) p2 stopped

auto :: (a -> b) -> Process1 a b
auto f = repeatedly $ do
    a <- await1
    yield (f a)
