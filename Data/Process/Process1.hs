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

import Prelude hiding (last)

import Control.Applicative ((<|>))
import Control.Monad
import Data.Foldable (Foldable, traverse_)
import Data.Monoid hiding ((<>))
import Data.Semigroup

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

echo :: Process1 a a
echo = auto id

prepended :: Foldable f => f a -> Process1 a a
prepended = before echo . traverse_ yield

filtered :: (a -> Bool) -> Process1 a a
filtered k = repeatedly $ do
    a <- await1
    when (k a) (yield a)

find :: (a -> Bool) -> Process1 a a
find k = process go
  where
    go = do
        a <- await1
        if k a then yield a else go

taking :: Int -> Process1 a a
taking i = process $ replicateM_ i (await1 >>= yield)

takingWhile :: (a -> Bool) -> Process1 a a
takingWhile k = repeatedly $ do
    a <- await1
    if k a then yield a else stop

dropping :: Int -> Process1 a a
dropping i = before echo $ replicateM_ i await1

last :: Process1 a a
last = process (await1 >>= go)
  where
    go prev = (await1 >>= go) <|> yield prev

scan :: (b -> a -> b) -> b -> Process1 a b
scan k z = process $ go z
  where
    go b = do
        yield b
        a <- await1
        go (k b a)

scan1 :: (a -> a -> a) -> Process1 a a
scan1 k = process (await1 >>= go)
  where
    go a1 = do
        yield a1
        a2 <- await1
        go (k a1 a2)

fold :: Monoid a => Process1 a a
fold = scan mappend mempty ~> last

fold1 :: Semigroup a => Process1 a a
fold1 = scan1 (<>) ~> last

foldMap :: Monoid b => (a -> b) -> Process1 a b
foldMap k = auto k ~> fold

foldMap1 :: Semigroup b => (a -> b) -> Process1 a b
foldMap1 k = auto k ~> fold1
