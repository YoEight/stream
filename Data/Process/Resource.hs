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

import System.IO.Error

import Data.Process.Plan

-- | not enough
resource :: IO r
         -> (r -> IO ())
         -> (r -> IO (Maybe o))
         -> Plan IO o ()
resource ack release step = onAwait go ack
  where
    go r = onAwaitFb (go1 r) (handle r) (cleanup r)

    handle r = catchIOError (step r) $ \e -> do
        release r
        ioError e

    go1 r (Just o) = yield o >> onAwaitFb (go1 r) (handle r) (cleanup r)
    go1 r _ = cleanup r

    cleanup r = await (release r)
