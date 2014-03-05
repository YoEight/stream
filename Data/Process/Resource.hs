{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Process.Resource
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
----------------------------------------------------------------------------
module Data.Process.Resource where

import Data.Process.Type

resource :: m r
         -> (r -> m ())
         -> (r -> m (Maybe o))
         -> Process m o
resource ack rel k =
    awaiting ack $ \r ->
        let onExit = awaiting (rel r) (const $ stopped) in
        loop (k r) onExit
  where
    loop step onExit =
        let inner Nothing  = onExit
            inner (Just o) = (yielding o) `append` (loop step onExit) in
        awaitingWith step inner onExit onExit
