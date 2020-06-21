module Language.Dickinson.Lib ( defaultLibPath
                              , libPath
                              ) where

import           Paths_language_dickinson (getDataDir)
import           System.FilePath          ((</>))

libPath :: IO [FilePath]
libPath = defaultLibPath <*> pure []

defaultLibPath :: IO ([FilePath] -> [FilePath])
defaultLibPath = do
    datadir <- getDataDir
    let preludeDir = datadir </> "prelude"
        libDir = datadir </> "lib"
    pure $ (preludeDir :) . (libDir :)
