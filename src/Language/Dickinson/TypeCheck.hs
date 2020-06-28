{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Dickinson.TypeCheck ( typeOf
                                    , tyAdd
                                    , tyTraverse
                                    , tyRun
                                    , emptyTyEnv
                                    , TyEnv
                                    , HasTyEnv (..)
                                    ) where

import           Control.Monad             (unless)
import           Control.Monad.Except      (ExceptT, MonadError, runExceptT, throwError)
import qualified Control.Monad.Ext         as Ext
import           Control.Monad.State       (MonadState, State, evalState)
import           Data.Binary               (Binary)
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

tyAssert :: (HasTyEnv s, MonadError (DickinsonError a) m, MonadState (s a) m) => DickinsonTy a -> Expression a -> m ()
tyAssert ty e = do
    ty' <- typeOf e
    unless (ty' == ty) $
        throwError (TypeMismatch e ty ty')

newtype TyEnv a = TyEnv { unTyEnv :: (IM.IntMap (DickinsonTy a)) }
    deriving (Binary)

class HasTyEnv f where
    tyEnvLens :: Lens' (f a) (IM.IntMap (DickinsonTy a))

instance HasTyEnv TyEnv where
    tyEnvLens f s = fmap (\x -> s { unTyEnv = x }) (f (unTyEnv s)) -- id

tyInsert :: (HasTyEnv s, MonadState (s a) m) => Name a -> DickinsonTy a -> m ()
tyInsert (Name _ (Unique i) _) ty = modifying tyEnvLens (IM.insert i ty)

tyMatch :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => NonEmpty (Expression a) -> m (DickinsonTy a)
tyMatch (e :| es) = do
    ty <- typeOf e
    traverse_ (tyAssert (TyText undefined)) es $> ty

type TypeM a = ExceptT (DickinsonError a) (State (TyEnv a))

tyRun :: [Declaration a] -> Either (DickinsonError a) ()
tyRun = flip evalState emptyTyEnv . runExceptT . (tyTraverse :: [Declaration a] -> TypeM a ())

emptyTyEnv :: TyEnv a
emptyTyEnv = TyEnv IM.empty

-- TODO: rename??
tyTraverse :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m ()
tyTraverse = traverse_ tyAdd

tyAdd :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => Declaration a -> m ()
tyAdd (Define _ n e) = tyInsert n =<< typeOf e

bindPattern :: (MonadState (s a) m, HasTyEnv s, MonadError (DickinsonError a) m) => Pattern a -> (DickinsonTy a) -> m ()
bindPattern (PatternVar _ n) ty                 = tyInsert n ty
bindPattern Wildcard{} _                        = pure ()
bindPattern (PatternTuple _ ps) (TyTuple _ tys) = Ext.zipWithM_ bindPattern ps tys -- FIXME: length ps = length tys
bindPattern (PatternTuple l _) _                = throwError $ MalformedTuple l

-- run after global renamer
typeOf :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => Expression a -> m (DickinsonTy a)
typeOf (Literal l _)  = pure (TyText l)
typeOf (StrChunk l _) = pure (TyText l)
typeOf (Choice _ brs) = tyMatch (snd <$> brs)
typeOf (Var l n@(Name _ (Unique i) _))  = do
    tyEnv <- use tyEnvLens
    case IM.lookup i tyEnv of
        Just ty -> pure ty
        Nothing -> throwError $ UnfoundName l n
typeOf (Interp l es) =
    traverse_ (tyAssert (TyText undefined)) es $> TyText l
typeOf (Concat l es) =
    traverse_ (tyAssert (TyText undefined)) es $> TyText l
typeOf (Lambda l n ty e) =
    tyInsert n ty *>
    (TyFun l ty <$> typeOf e)
typeOf (Tuple l es) = TyTuple l <$> traverse typeOf es
typeOf (Apply _ e e') = do
    ty <- typeOf e
    case ty of
        TyFun _ ty' ty'' -> do
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
