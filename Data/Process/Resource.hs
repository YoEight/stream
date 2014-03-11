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

import System.IO

import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text.IO    as T

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

sinkFile :: FilePath -> Sink IO BS.ByteString
sinkFile path = resource ack hClose step
  where
    ack = openFile path WriteMode

    step h = return $ Just $ BS.hPut h

sourceFile :: FilePath -> Process IO BS.ByteString
sourceFile path = resource ack hClose step
  where
    ack = openFile path ReadMode

    step h = do
        eof <- hIsEOF h
        if eof then return Nothing else fmap Just (BS.hGet h 8192)

lines :: FilePath -> Process IO Text
lines path = resource ack hClose step
  where
    ack = openFile path ReadMode

    step h = do
        eof <- hIsEOF h
        if eof then return Nothing else fmap Just (T.hGetLine h)
