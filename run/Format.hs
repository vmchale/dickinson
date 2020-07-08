module Format ( fmtFile
              , fmtInplace
              ) where

import           Control.Monad               ((<=<))
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Text.IO                as TIO
import           Language.Dickinson.Pipeline

fmtFile :: FilePath -> IO ()
fmtFile = TIO.putStrLn . format <=< BSL.readFile

fmtInplace :: FilePath -> IO ()
fmtInplace fp = TIO.writeFile fp . format . BSL.fromStrict =<< BS.readFile fp
