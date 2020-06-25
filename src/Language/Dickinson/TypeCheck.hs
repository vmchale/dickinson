{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Dickinson.TypeCheck ( typeOf
                                    , tyAdd
                                    , tyTraverse
                                    , tyRun
                                    , TyEnv
                                    , HasTyEnv (..)
                                    ) where

import           Control.Monad             (unless)
import           Control.Monad.Except      (ExceptT, MonadError, runExceptT, throwError)
import qualified Control.Monad.Ext         as Ext
import           Control.Monad.State       (MonadState, State, evalState)
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

instance HasTyEnv (IM.IntMap DickinsonTy) where
    tyEnvLens = id

tyInsert :: (HasTyEnv s, MonadState s m) => Name a -> DickinsonTy -> m ()
tyInsert (Name _ (Unique i) _) ty = modifying tyEnvLens (IM.insert i ty)

tyMatch :: (HasTyEnv s, MonadState s m, MonadError (DickinsonError a) m) => NonEmpty (Expression a) -> m DickinsonTy
tyMatch (e :| es) = do
    ty <- typeOf e
    traverse_ (tyAssert TyText) es $> ty

type TypeM a = ExceptT (DickinsonError a) (State TyEnv)

tyRun :: Dickinson a -> Either (DickinsonError a) ()
tyRun = flip evalState mempty . runExceptT . (tyTraverse :: Dickinson a -> TypeM a ())

tyTraverse :: (HasTyEnv s, MonadState s m, MonadError (DickinsonError a) m) => Dickinson a -> m ()
tyTraverse (Dickinson _ ds) = traverse_ tyAdd ds

tyAdd :: (HasTyEnv s, MonadState s m, MonadError (DickinsonError a) m) => Declaration a -> m ()
tyAdd (Define _ n e) = tyInsert n =<< typeOf e

bindPattern :: (MonadState s m, HasTyEnv s, MonadError (DickinsonError a) m) => Pattern a -> DickinsonTy -> m ()
bindPattern (PatternVar _ n) ty               = tyInsert n ty
bindPattern Wildcard{} _                      = pure ()
bindPattern (PatternTuple _ ps) (TyTuple tys) = Ext.zipWithM_ bindPattern ps tys -- FIXME: length ps = length tys
bindPattern (PatternTuple l _) _              = throwError $ MalformedTuple l

-- run after global renamer
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
typeOf (Concat _ es) =
    traverse_ (tyAssert TyText) es $> TyText
typeOf (Lambda _ n ty e) =
    tyInsert n ty *>
    (TyFun ty <$> typeOf e)
typeOf (Tuple _ es) = TyTuple <$> traverse typeOf es
typeOf (Apply _ e e') = do
    ty <- typeOf e
    case ty of
        TyFun ty' ty'' -> do
            tyAssert ty' e'
            pure ty''
        _ -> throwError $ ExpectedLambda e ty
typeOf (Let _ bs e) = do
    es' <- traverse typeOf (snd <$> bs)
    let ns = fst <$> bs
    Ext.zipWithM_ tyInsert ns es'
    typeOf e
typeOf (Match _ e p e') = do
    ty <- typeOf e
    bindPattern p ty
    typeOf e'
typeOf (Flatten _ e) = typeOf e
typeOf (Annot _ e ty) =
    tyAssert ty e $> ty
