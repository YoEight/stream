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
module Data.Process.Resource (Res, resource, runResIO) where

import Control.Exception (finally)
import Data.Foldable (sequenceA_)
import Data.IORef

import qualified Data.IntMap.Strict as MS

import Data.Process.Plan

newtype ReleaseKey = ReleaseKey Int

newtype Res m a
    = Res
      { unRes ::
             forall x.
             (a -> x) ->                                                      -- done
             (forall r. m r -> (r -> x) -> x) ->                              -- run effect
             (forall r. m r -> (r -> m ()) -> (r -> ReleaseKey -> x) -> x) -> -- allocation
             (ReleaseKey -> x -> x) ->                                        -- release
             x
      }

instance Monad (Res m) where
    return a = Res $ \kd _ _ _ -> kd a

    Res k >>= f =
        Res $ \kd krun kreg krel ->
            let done a = unRes (f a) kd krun kreg krel in
            k done krun kreg krel

allocate :: m r -> (r -> m ()) -> Res m (r, ReleaseKey)
allocate ack rel = Res $ \kd _ kreg _ -> kreg ack rel (\r i -> kd (r, i))

release :: ReleaseKey -> Res m ()
release key = Res $ \kd _ _ krel -> krel key (kd ())

onEffect :: m r -> Res m r
onEffect rq = Res $ \kd krun _ _ -> krun rq kd

runResIO :: Res IO a -> IO a
runResIO (Res k) = do
    ref <- newIORef MS.empty
    let doDone :: a -> (Int -> IO a)
        doDone a _ = return a

        doEffect :: IO r
                 -> (r -> (Int -> IO a))
                 -> (Int -> IO a)
        doEffect rq k i = do
            r <- rq
            k r i

        doAlloc :: IO r
                -> (r -> IO ())
                -> (r -> ReleaseKey -> (Int -> IO a))
                -> (Int -> IO a)
        doAlloc ack kr k i = do
            r <- ack
            modifyIORef' ref (MS.insert i (kr r))
            k r (ReleaseKey i) (i+1)

        doRelease :: ReleaseKey
                  -> (Int -> IO a)
                  -> (Int -> IO a)
        doRelease (ReleaseKey key) k i = do
            m <- readIORef ref
            let (opt, m2) = MS.updateLookupWithKey (\_ _ -> Nothing) key m
            sequenceA_ opt
            writeIORef ref m2
            k i

    finally (k doDone doEffect doAlloc doRelease $ 0) $ do
        m <- readIORef ref
        sequenceA_ (MS.elems m)

resource :: m r
         -> (r -> m ())
         -> (r -> m (Maybe o))
         -> Plan (Res m) o ()
resource ack rel k = do
    (r,key) <- await $ allocate ack rel
    loop key r
  where
    loop key r = do
        opt <- await $ onEffect (k r)
        case opt of
            Nothing -> await $ release key
            Just o  -> yield o >> loop key r
