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

preludeLibPath :: FilePath -> [FilePath] -> [FilePath]
preludeLibPath fp = (preludeDir :) . (libDir :)
    where preludeDir = fp </> "prelude"
          libDir = fp </> "lib"

homeMod :: IO ([FilePath] -> [FilePath])
homeMod = do
    mHome <- lookupEnv "HOME"
    pure $ case mHome of
        Just h  -> preludeLibPath (h </> ".emd")
        Nothing -> id

defaultLibPath :: IO ([FilePath] -> [FilePath])
defaultLibPath = do
    datadir <- getDataDir
    (.) (preludeLibPath datadir) <$> homeMod
