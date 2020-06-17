-- | This modules contains some bits and pieces to work with Dickinson code.
module Language.Dickinson ( -- * Parser
                            parse
                          , parseWithCtx
                          , parseExpressionWithCtx
                          , ParseError (..)
                          -- * Lexer
                          , AlexPosn
                          , lexDickinson
                          , Token
                          , AlexUserState
                          -- * Renamer
                          , maxLens
                          , initRenames
                          , renameDickinson
                          , renameDickinsonM
                          , renameExpressionM
                          , UniqueCtx
                          , Renames
                          , HasRenames (..)
                          -- * Checks/passes
                          , checkMultiple
                          , checkScope
                          , checkFile
                          -- * AST
                          , Dickinson
                          , Declaration (..)
                          , Expression (..)
                          , DickinsonTy (..)
                          , Name (..)
                          , Unique
                          , dummyUnique
                          -- * Evaluation and Environments
                          , loadDickinson
                          , evalExpressionM
                          , evalFile
                          , EvalM
                          , EvalSt (..)
                          , HasEvalSt (..)
                          -- * Import resolution
                          , resolveImport
                          -- * Errors
                          , DickinsonError (..)
                          -- * Pretty-printing
                          , prettyDickinson
                          -- * Version info
                          , languageDickinsonVersion
                          , languageDickinsonVersionString
                          ) where

import           Control.Exception             (Exception, throw, throwIO)
import           Control.Monad                 ((<=<))
import           Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as T
import qualified Data.Version                  as V
import           Language.Dickinson.Check
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Pretty
import           Language.Dickinson.Rename
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import qualified Paths_language_dickinson      as P

-- | Check scoping
checkFile :: FilePath -> IO ()
checkFile = h . go <=< BSL.readFile
    where go = checkScope . fst . uncurry renameDickinson . yeet . parseWithCtx
          h (Just err) = throwIO err
          h Nothing    = pure ()

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
