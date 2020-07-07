{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Pipeline ( tcIO
                                   , checkEvalM
                                   ) where

import           Control.Exception.Value       (eitherThrowIO)
import           Control.Monad.Except          (MonadError)
import           Control.Monad.State.Lazy      (MonadState)
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Pretty)
import           Data.Typeable                 (Typeable)
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck

tcIO :: (Typeable a, Pretty a) => [Declaration a] -> IO ()
tcIO = eitherThrowIO . tyRun

checkEvalM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m T.Text
checkEvalM ds = do
    maybeThrow $ checkScope ds
    tyTraverse ds
    evalDickinsonAsMain ds
