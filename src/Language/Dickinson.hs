-- | This modules contains some bits and pieces to work with Dickinson code.
module Language.Dickinson ( -- * Parser
                            parse
                          , parseWithMax
                          , parseWithCtx
                          , parseReplWithCtx
                          , ParseError (..)
                          -- * Lexer
                          , AlexPosn
                          , lexDickinson
                          , Token
                          , AlexUserState
                          , alexInitUserState
                          -- * Renamer
                          , maxLens
                          , balanceMax
                          , initRenames
                          , addDecl
                          , renameDickinson
                          , renameDickinsonM
                          , renameDeclarationM
                          , renameExpressionM
                          , UniqueCtx
                          , Renames
                          , HasRenames (..)
                          -- * Checks/passes
                          , checkMultiple
                          , checkDuplicates
                          , checkScope
                          , checkFile
                          , warnFile
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
                          , lexerStateLens
                          , EvalM
                          , EvalSt (..)
                          -- * Type inference
                          , tcFile
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

import           Control.Applicative               ((<|>))
import           Control.Exception                 (Exception, throw, throwIO)
import           Control.Monad                     ((<=<))
import           Data.Bifunctor                    (second)
import           Data.ByteString.Lazy              as BSL
import qualified Data.Text                         as T
import qualified Data.Version                      as V
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
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
import           Language.Dickinson.TypeCheck
import           Language.Dickinson.Unique
import qualified Paths_language_dickinson          as P

-- | Check scoping
checkFile :: FilePath -> IO ()
checkFile = h . go <=< BSL.readFile
    where go = checkScope . fst . uncurry renameDickinson . yeet . parseWithMax
          h (Just err) = throwIO err
          h Nothing    = pure ()

-- | Run some lints
warnFile :: FilePath -> IO ()
warnFile = h . go <=< BSL.readFile
    where go = checks . yeet . parse
          h (Just err) = throwIO err
          h Nothing    = pure ()
          checks x = checkDuplicates x <|> checkMultiple x

tcFile :: FilePath -> IO ()
tcFile = h . go <=< BSL.readFile
    where go = tyRun . fst . uncurry renameDickinson . yeet . parseWithMax
          h Right{}    = pure ()
          h (Left err) = throwIO err

-- TODO: runDeclarationM
evalFile :: FilePath -> IO T.Text
evalFile = fmap yeet . uncurry evalIO . second evalDickinsonAsMain . yeet . flip parseWithCtx alexInitUserState <=< BSL.readFile
-- TODO: renameDickinson

yeet :: Exception e => Either e x -> x
yeet = either throw id

languageDickinsonVersion :: V.Version
languageDickinsonVersion = P.version

languageDickinsonVersionString :: String
languageDickinsonVersionString = V.showVersion languageDickinsonVersion
