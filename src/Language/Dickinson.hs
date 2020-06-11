module Language.Dickinson ( parse
                          , parseWithCtx
                          , lexDickinson
                          , prettyDickinson
                          , renameDickinson
                          -- * Types
                          , Dickinson
                          , Declaration (..)
                          , Name (..)
                          , evalFile
                          -- * Reëxports from
                          -- "Data.Text.Prettyprint.Doc.Render.Text"
                          , renderLazy
                          , renderStrict
                          , Pretty (pretty)
                          , putDoc
                          ) where

import           Control.Monad                         ((<=<))
import           Data.ByteString.Lazy                  as BSL
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (Pretty (pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc, renderLazy,
                                                        renderStrict)
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Pretty
import           Language.Dickinson.Rename
import           Language.Dickinson.Type

-- TODO: runDeclarationM
evalFile :: FilePath -> IO T.Text
evalFile = fmap yeet . evalIO (initRenames 1000) . evalExpressionM . yeet . findMain . yeet . parse <=< BSL.readFile
-- TODO: renameDickinson

yeet :: Show a => Either a x -> x
yeet = either (error.show) id
