{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.TypeCheck ( typeOf
                                    , TyEnv
                                    , HasTyEnv (..)
                                    ) where

import           Control.Monad             (unless)
import           Control.Monad.Except      (ExceptT, MonadError, throwError)
import           Control.Monad.State       (MonadState, State, get, modify)
import           Data.Foldable             (traverse_)
import           Data.Functor              (($>))
import qualified Data.IntMap               as IM
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                (Lens')
import           Lens.Micro.Mtl            (modifying, use)

tyAssert :: (HasTyEnv s, MonadError (DickinsonError a) m, MonadState s m) => DickinsonTy -> Expression a -> m ()
tyAssert ty e = do
    ty' <- typeOf e
    unless (ty' == ty) $
        throwError (TypeMismatch e ty ty')

type TyEnv = IM.IntMap DickinsonTy

class HasTyEnv a where
    tyEnvLens :: Lens' a TyEnv

tyInsert :: (HasTyEnv s, MonadState s m) => Name a -> DickinsonTy -> m ()
tyInsert (Name _ (Unique i) _) ty = modifying tyEnvLens (IM.insert i ty)

tyMatch :: (HasTyEnv s, MonadState s m, MonadError (DickinsonError a) m) => NonEmpty (Expression a) -> m DickinsonTy
tyMatch (e :| es) = do
    ty <- typeOf e
    traverse_ (tyAssert TyText) es $> ty

-- run after global renamer &c.
typeOf :: (HasTyEnv s, MonadState s m, MonadError (DickinsonError a) m) => Expression a -> m DickinsonTy
typeOf Literal{}  = pure TyText
typeOf StrChunk{} = pure TyText
typeOf (Choice _ brs) = tyMatch (snd <$> brs)
typeOf (Var l n@(Name _ (Unique i) _))  = do
    tyEnv <- use tyEnvLens
    case IM.lookup i tyEnv of
        Just ty -> pure ty
        Nothing -> throwError $ UnfoundName l n
typeOf (Interp _ es) =
    traverse_ (tyAssert TyText) es $> TyText
typeOf (Lambda _ n ty e) =
    tyInsert n ty *>
    (TyFun ty <$> typeOf e)
typeOf (Tuple _ es) = TyTuple <$> traverse typeOf es
