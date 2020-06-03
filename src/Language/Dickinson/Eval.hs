{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Eval ( EvalM
                               ) where

import           Control.Monad.Except     (ExceptT, runExceptT, throwError)
import           Control.Monad.State      (StateT, evalStateT, gets, modify)
import qualified Data.IntMap              as IM
import           Data.Semigroup           (sconcat)
import qualified Data.Text                as T
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

-- map to expression
type EvalSt name a = IM.IntMap (Expression name a)

type EvalM name a = StateT (EvalSt name a) (ExceptT (DickinsonError name a) IO)

runEvalSt :: EvalM name a x -> IO (Either (DickinsonError name a) x)
runEvalSt = runExceptT . flip evalStateT mempty

bindName :: Name a -> Expression Name a -> EvalM Name a ()
bindName (Name _ (Unique u) _) e = modify (IM.insert u e)

deleteName :: Name a -> EvalM Name a ()
deleteName (Name _ (Unique u) _) = modify (IM.delete u)

lookupName :: Name a -> EvalM Name a (Expression Name a)
lookupName n@(Name _ (Unique u) l) = go =<< gets (IM.lookup u)
    where go Nothing  = throwError (UnfoundName l n)
          go (Just x) = pure x

-- TODO: clone identifiers
evalM :: Expression Name a -> EvalM Name a T.Text
evalM (Literal _ t) = pure t
evalM (Concat _ es) = sconcat <$> traverse evalM es
evalM (Var _ n)     = evalM =<< lookupName n
