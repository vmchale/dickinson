module Language.Dickinson.Import ( resolveImport
                                 ) where

import           Control.Monad           (filterM)
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as T
import           Language.Dickinson.Name
import           System.Directory        (doesFileExist)
import           System.FilePath         ((</>))

-- TODO: dependency analysis

resolveImport :: [FilePath] -- ^ Places to look
              -> Name a
              -> IO (Maybe FilePath)
resolveImport incl n = fmap listToMaybe .
    filterM doesFileExist .
    fmap (</> getFileName n) $ incl

getFileName :: Name a -> FilePath
getFileName = (<> ".dck") . foldr (</>) mempty . fmap (T.unpack) . name
