module Language.Dickinson.TypeCheck ( typeOf
                                    ) where

import           Control.Monad             (unless, void)
import           Control.Monad.Except      (ExceptT, throwError)
import           Control.Monad.State       (State, StateT, get, modify)
import           Data.Foldable             (traverse_)
import           Data.Functor              (($>))
import qualified Data.IntMap               as IM
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique

tyAssert :: DickinsonTy () -> Expression Name () -> TypeM ()
tyAssert ty e = do
    ty' <- typeOf e
    unless (ty' == ty) $
        throwError (TypeMismatch e ty ty')

type TyEnv = IM.IntMap (DickinsonTy ())

type TypeM = ExceptT (DickinsonError Name ()) (State TyEnv)

tyInsert :: Name a -> DickinsonTy () -> TypeM ()
tyInsert (Name _ (Unique i) _) ty = modify (IM.insert i ty)

-- run after global renamer &c.
typeOf :: Expression Name () -> TypeM (DickinsonTy ())
typeOf Literal{}  = pure $ TyText ()
typeOf StrChunk{} = pure $ TyText ()
typeOf (Var l n@(Name _ (Unique i) _))  = do
    tyEnv <- get
    case IM.lookup i tyEnv of
        Just ty -> pure ty
        Nothing -> throwError $ UnfoundName l n
typeOf (Interp _ es) =
    traverse_ (tyAssert $ TyText ()) es $> TyText ()
typeOf (Lambda _ n ty e) = do
    tyInsert n ty
    TyFun () ty <$> typeOf e
