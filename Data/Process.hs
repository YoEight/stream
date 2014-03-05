-----------------------------------------------------------------------------
-- |
-- Module : Data.Process
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
----------------------------------------------------------------------------
module Data.Process
       ( module Data.Process.Plan
       , module Data.Process.Resource
       , module Data.Process.Tee
       , module Data.Process.Type
       , collectProcess
       , runProcess
       , through
       , source
       ) where

import Prelude hiding (zipWith)

import Control.Monad (liftM)
import Data.Foldable

import Control.Monad.Catch
import Data.Process.Plan
import Data.Process.Resource
import Data.Process.Tee
import Data.Process.Type

through :: Process m a -> Channel a m b -> Process m b
through p1 p2 = eval $ tee (zipWith (\a f -> f a)) p1 p2

source :: Foldable f => f a -> Process m a
source = process . traverse_ yield

runProcess :: MonadCatch m => Process m o -> m ()
runProcess = liftM (const ()) . collectProcess

collectProcess :: MonadCatch m => Process m o -> m [o]
collectProcess (Process m) =
    case m of
        Stop e          -> maybe (return []) throwM e
        Yield o n       -> liftM (o:) (collectProcess n)
        Await rq k _ cl -> do
            t_e <- try rq
            collectProcess $ either (causedBy cl . Just) k t_e
