{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Pipeline ( checkEvalM
                                   , format
                                   , validateDecl
                                   ) where

import           Control.Exception.Value        (eitherThrow)
import           Control.Monad.Except           (MonadError)
import           Control.Monad.State.Lazy       (MonadState)
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.Text                      as T
import           Data.Text.Prettyprint.Doc.Ext  (prettyText)
import           Language.Dickinson.Check.Scope
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.Parser
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck

-- this one is faster with (*>) than 'do'
checkEvalM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m T.Text
checkEvalM ds =
    validateDecl ds *>
    evalDickinsonAsMain ds

format :: BSL.ByteString -> T.Text
format = prettyText . eitherThrow . parse

-- the 'do'-notation is faster than using *>
validateDecl :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m ()
validateDecl d = do
    maybeThrow $ checkScope d
    tyTraverse d
