{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Process.Wye
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
----------------------------------------------------------------------------
module Data.Process.Wye where

import Data.Process.Plan
import Data.Process.Type

data T a b c where
    L :: T a b a
    R :: T a b b
    Y :: T a b (Either a b)

type Wye a b c = Process (T a b) c

awaitL :: Plan (T a b) o a
awaitL = await L

awaitR :: Plan (T a b) o b
awaitR = await R

awaitBoth :: Plan (T a b) o (Either a b)
awaitBoth = await Y
