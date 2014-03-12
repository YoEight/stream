{-# LANGUAGE OverloadedStrings #-}
module Example where

import Prelude hiding (lines)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.Process

copy_file :: FilePath -> FilePath -> IO ()
copy_file from to = runProcess $ sourceFile from `through` sinkFile to

converter :: Process IO ()
converter = lines "testdata/fahrenheit.txt"                        ~>
            filtered (\s -> not (T.null s || T.isPrefixOf "//" s)) ~>
            stod                                                   ~>
            auto fahrenheitToCelsius                               ~>
            dtos                                                   ~>
            auto T.encodeUtf8                                      `through`
            sinkFile "testdata/celsius.txt"

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius = undefined

stod :: Process1 T.Text Double
stod =  undefined

dtos :: Process1 Double T.Text
dtos = undefined
