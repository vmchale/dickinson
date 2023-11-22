{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Dickinson.TypeCheck ( typeOf
                                    , tyAdd
                                    , tyAddDecl
                                    , tyTraverse
                                    , tyRun
                                    , emptyTyEnv
                                    , runTypeM
                                    , TypeM
                                    , TyEnv
                                    , HasTyEnv (..)
                                    ) where

import           Control.Monad             (forM_, unless)
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

newtype TyEnv a = TyEnv { unTyEnv :: IM.IntMap (DickinsonTy a) }
    deriving (Binary)

class HasTyEnv f where
    tyEnvLens :: Lens' (f a) (IM.IntMap (DickinsonTy a))

instance HasTyEnv TyEnv where
    tyEnvLens f s = fmap (\x -> s { unTyEnv = x }) (f (unTyEnv s)) -- id

tyInsert :: (HasTyEnv s, MonadState (s a) m) => Name a -> DickinsonTy a -> m ()
tyInsert (Name _ (Unique i) _) ty = {-# SCC "tyInsert" #-} modifying tyEnvLens (IM.insert i ty)

tyMatch :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => NonEmpty (Expression a) -> m (DickinsonTy a)
tyMatch (e :| es) = do
    ty <- typeOf e
    traverse_ (tyAssert ty) es $> ty

type TypeM a = ExceptT (DickinsonError a) (State (TyEnv a))

tyRun :: [Declaration a] -> Either (DickinsonError a) ()
tyRun = runTypeM . tyTraverse

runTypeM :: TypeM a x -> Either (DickinsonError a) x
runTypeM = flip evalState emptyTyEnv . runExceptT

emptyTyEnv :: TyEnv a
emptyTyEnv = TyEnv IM.empty

tyTraverse :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => [Declaration a] -> m ()
tyTraverse ds =
    traverse_ tyAddDecl ds *>
    traverse_ tyAdd ds

tyAdd :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => Declaration a -> m ()
tyAdd (Define _ n e) = tyInsert n =<< typeOf e
tyAdd TyDecl{}       = pure ()

tyAddDecl :: (HasTyEnv s, MonadState (s a) m) => Declaration a -> m ()
tyAddDecl Define{}         = pure ()
tyAddDecl (TyDecl l tn cs) = traverse_ (\c -> tyInsert c (TyNamed l tn)) cs

bindPattern :: (MonadState (s a) m, HasTyEnv s, MonadError (DickinsonError a) m) => Pattern a -> DickinsonTy a -> m ()
bindPattern (PatternVar _ n) ty                 = tyInsert n ty
bindPattern Wildcard{} _                        = pure ()
bindPattern (PatternTuple l ps) (TyTuple _ tys)
    | length ps == length tys = Ext.zipWithM_ bindPattern ps tys
    | otherwise = throwError $ MalformedTuple l
bindPattern (PatternTuple l _) _                = throwError $ MalformedTuple l
bindPattern p@(PatternCons l tn@(Name _ (Unique k) _)) ty = do
    tyEnv <- use tyEnvLens
    case IM.lookup k tyEnv of
        Just ty' ->
            unless (ty' == ty) $
                throwError (PatternTypeMismatch p ty ty')
        Nothing -> throwError $ UnfoundConstructor l tn
bindPattern (OrPattern _ ps) ty =
    traverse_ (\p -> bindPattern p ty) ps

-- run after global renamer
typeOf :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m) => Expression a -> m (DickinsonTy a)
typeOf (Literal l _)  = pure (TyText l)
typeOf (StrChunk l _) = pure (TyText l)
typeOf (Choice _ brs) = tyMatch (snd <$> brs)
typeOf (Var l n@(Name _ (Unique i) _)) = do
    tyEnv <- use tyEnvLens
    case IM.lookup i tyEnv of
        Just ty -> pure ty
        Nothing -> throwError $ UnfoundName l n
typeOf (MultiInterp l es) =
    traverse_ (tyAssert (TyText undefined)) es $> TyText l
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
typeOf (Let _ bs e) = tyLet bs e
typeOf (Bind _ bs e) = tyLet bs e
typeOf (Match _ e brs@((_,e') :| _)) = do
    ty <- typeOf e
    forM_ (fst <$> brs) $ \p ->
        {-# SCC "bindPattern" #-} bindPattern p ty
    res <- typeOf e'
    traverse_ (tyAssert res.snd) brs
    pure res
typeOf (Flatten _ e) = typeOf e
typeOf (Annot _ e ty) =
    tyAssert ty e $> ty
typeOf (Constructor l tn@(Name _ (Unique k) _)) = do
    tyEnv <- use tyEnvLens
    case IM.lookup k tyEnv of
        Just ty -> pure ty
        Nothing -> throwError $ UnfoundConstructor l tn
typeOf (BuiltinFn l _) = pure $ -- all builtins have type (-> text text)
    TyFun l (TyText l) (TyText l)
typeOf (Random l n) = pure $ TyNamed l n

-- since :let and :bind are the same in this scheme
tyLet :: (HasTyEnv s, MonadState (s a) m, MonadError (DickinsonError a) m)
      => NonEmpty (Name a, Expression a)
      -> Expression a
      -> m (DickinsonTy a)
tyLet bs e = do
    es' <- traverse (typeOf.snd) bs
    let ns = fst <$> bs
    Ext.zipWithM_ tyInsert ns es'
    typeOf e
