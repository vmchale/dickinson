{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Eval ( EvalM
                               ) where

import           Control.Monad.Except     (ExceptT, throwError)
import           Control.Monad.State      (StateT, gets, modify)
import qualified Data.IntMap              as IM
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

-- map to expression
type EvalSt name a = IM.IntMap (Expression name a)

type EvalM name a = ExceptT (DickinsonError name a) (StateT (EvalSt name a) IO)

bindName :: Name a -> Expression Name a -> EvalM Name a ()
bindName (Name _ (Unique u) _) e = modify (IM.insert u e)

deleteName :: Name a -> EvalM Name a ()
deleteName (Name _ (Unique u) _) = modify (IM.delete u)

lookupName :: Name a -> EvalM Name a (Expression Name a)
lookupName n@(Name _ (Unique u) l) = go =<< gets (IM.lookup u)
    where go Nothing  = throwError (UnfoundName l n)
          go (Just x) = pure x

-- lol oulipo/to uppercase &c.
