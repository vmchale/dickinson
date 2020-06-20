module Language.Dickinson.File ( evalFile
                               , checkFile
                               , warnFile
                               , tcFile
                               ) where

import Language.Dickinson.Parser
import Language.Dickinson.Lexer
import Language.Dickinson.Eval
import Language.Dickinson.TypeCheck
import Language.Dickinson.Check
import Language.Dickinson.ScopeCheck
import Language.Dickinson.DuplicateCheck
import Language.Dickinson.Rename
import Data.Bifunctor (second)
import Control.Applicative ((<|>))
import Control.Exception (Exception, throwIO, throw)
import qualified Data.ByteString.Lazy as BSL
import Control.Monad ((<=<))
import Data.Text as T

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
