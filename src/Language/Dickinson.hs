module Language.Dickinson ( parse
                          , parseWithCtx
                          , lexDickinson
                          , prettyDickinson
                          , renameDickinson
                          , checkMultiple
                          -- * Types
                          , Dickinson
                          , Declaration (..)
                          , Name (..)
                          , UniqueCtx
                          , evalFile
                          -- * Reëxports from
                          -- "Data.Text.Prettyprint.Doc.Render.Text"
                          , renderLazy
                          , renderStrict
                          , Pretty (pretty)
                          , putDoc
                          -- * Version info
                          , languageDickinsonVersion
                          , languageDickinsonVersionString
                          ) where

import           Control.Exception                     (Exception, throw)
import           Control.Monad                         ((<=<))
import           Data.ByteString.Lazy                  as BSL
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (Pretty (pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc, renderLazy, renderStrict)
import           Language.Dickinson.Check
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Pretty
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import qualified Data.Version as V
import qualified Paths_language_dickinson as P

-- TODO: runDeclarationM
evalFile :: FilePath -> IO T.Text
evalFile = fmap yeet . evalIO 1000 . evalDickinsonAsMain . yeet . parse <=< BSL.readFile
-- TODO: renameDickinson

yeet :: Exception e => Either e x -> x
yeet = either throw id

languageDickinsonVersion :: V.Version
languageDickinsonVersion = P.version

languageDickinsonVersionString :: String
languageDickinsonVersionString = V.showVersion languageDickinsonVersion
