module Language.Dickinson.Lib ( defaultLibPath
                              , dckPath
                              ) where

import           Data.List.Split          (splitWhen)
import           Paths_language_dickinson (getDataDir)
import           System.Environment       (lookupEnv)
import           System.FilePath          ((</>))

dckPath :: IO [FilePath]
dckPath = maybe [] splitEnv <$> lookupEnv "DCK_PATH"

splitEnv :: String -> [FilePath]
splitEnv = splitWhen (== ':')

defaultLibPath :: IO ([FilePath] -> [FilePath])
defaultLibPath = do
    datadir <- getDataDir
    let preludeDir = datadir </> "prelude"
        libDir = datadir </> "lib"
    pure $ (preludeDir :) . (libDir :)
