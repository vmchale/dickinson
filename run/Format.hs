module Format ( fmtFile
              , fmtInplace
              ) where

import           Control.Exception.Value       (eitherThrow)
import           Control.Monad                 ((<=<))
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as T
import           Data.Text.IO                  as TIO
import           Data.Text.Prettyprint.Doc.Ext (prettyText)
import           Language.Dickinson.Parser

format :: BSL.ByteString -> T.Text
format = prettyText . eitherThrow . parse

fmtFile :: FilePath -> IO ()
fmtFile = TIO.putStrLn . format <=< BSL.readFile

fmtInplace :: FilePath -> IO ()
fmtInplace fp = TIO.writeFile fp . format . BSL.fromStrict =<< BS.readFile fp
