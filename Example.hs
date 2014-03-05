module Example where

import Data.Process

copy_file :: FilePath -> FilePath -> IO ()
copy_file from to = runProcess $ sourceFile from `through` sinkFile to
