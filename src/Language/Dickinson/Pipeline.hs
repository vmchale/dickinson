{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Pipeline ( checkEvalM
                                   , validateDecl
                                   ) where

import           Control.Monad.Except           (MonadError)
import           Control.Monad.State.Lazy       (MonadState)
import qualified Data.Text                      as T
import           Language.Dickinson.Check.Scope
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck

-- this one is faster with (*>) than 'do'
checkEvalM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m T.Text
checkEvalM ds =
    validateDecl ds *>
    evalDickinsonAsMain ds

-- the 'do'-notation is faster than using *>
validateDecl :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m ()
validateDecl d = do
    maybeThrow $ checkScope d
    tyTraverse d
