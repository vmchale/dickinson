{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Pipeline ( checkEvalM
                                   , format
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

checkEvalM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m T.Text
checkEvalM ds = do
    maybeThrow $ checkScope ds
    tyTraverse ds
    evalDickinsonAsMain ds

format :: BSL.ByteString -> T.Text
format = prettyText . eitherThrow . parse
