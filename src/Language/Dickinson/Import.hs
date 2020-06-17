module Language.Dickinson.Import ( resolveImport
                                 ) where

import           Control.Monad           (filterM)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as T
import           Language.Dickinson.Name
import           System.Directory        (doesFileExist)
import           System.FilePath         ((</>))

-- TODO: dependency analysis

resolveImport :: MonadIO m
              => [FilePath] -- ^ Places to look
              -> Name a
              -> m (Maybe FilePath)
resolveImport incl n = liftIO
    . fmap listToMaybe
    . filterM doesFileExist
    . fmap (</> getFileName n) $ incl

getFileName :: Name a -> FilePath
getFileName = (<> ".dck") . foldr (</>) mempty . fmap T.unpack . name
