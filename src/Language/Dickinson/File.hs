module Language.Dickinson.File ( evalFile
                               , checkFile
                               , warnFile
                               , tcFile
                               , fmtFile
                               ) where

import           Control.Applicative                   ((<|>))
import           Control.Exception                     (Exception, throw, throwIO)
import           Control.Monad                         ((<=<))
import           Data.Bifunctor                        (second)
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Semigroup                        ((<>))
import           Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (hardline)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Language.Dickinson.Check
import           Language.Dickinson.DuplicateCheck
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer
import           Language.Dickinson.Parser
import           Language.Dickinson.Pretty
import           Language.Dickinson.Rename
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.TypeCheck

fmtFile :: FilePath -> IO ()
fmtFile = putDoc . (<> hardline) . prettyDickinson . go <=< BSL.readFile
    where go = fst . uncurry renameDickinson . yeet . parseWithMax

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
evalFile :: [FilePath] -> FilePath -> IO T.Text
evalFile is = fmap yeet . uncurry evalIO . second (evalDickinsonAsMain is) . yeet . flip parseWithCtx alexInitUserState <=< BSL.readFile
-- TODO: renameDickinson

yeet :: Exception e => Either e x -> x
yeet = either throw id
