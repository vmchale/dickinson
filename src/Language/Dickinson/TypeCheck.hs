module Language.Dickinson.TypeCheck ( typeOf
                                    ) where

import           Control.Monad             (unless)
import           Control.Monad.Except      (ExceptT, throwError)
import           Control.Monad.State       (State, get, modify)
import           Data.Foldable             (traverse_)
import           Data.Functor              (($>))
import qualified Data.IntMap               as IM
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique

tyAssert :: DickinsonTy () -> Expression () -> TypeM ()
tyAssert ty e = do
    ty' <- typeOf e
    unless (ty' == ty) $
        throwError (TypeMismatch e ty ty')

type TyEnv = IM.IntMap (DickinsonTy ())

type TypeM = ExceptT (DickinsonError ()) (State TyEnv)

tyInsert :: Name a -> DickinsonTy () -> TypeM ()
tyInsert (Name _ (Unique i) _) ty = modify (IM.insert i ty)

tyDelete :: Name a -> TypeM ()
tyDelete (Name _ (Unique i) _) = modify (IM.delete i)

tyMatch :: NonEmpty (Expression ()) -> TypeM (DickinsonTy ())
tyMatch (e :| es) = do
    ty <- typeOf e
    traverse_ (tyAssert $ TyText ()) es $> ty

-- run after global renamer &c.
typeOf :: Expression () -> TypeM (DickinsonTy ())
typeOf Literal{}  = pure $ TyText ()
typeOf StrChunk{} = pure $ TyText ()
typeOf (Choice _ brs) = tyMatch (snd <$> brs)
typeOf (Var l n@(Name _ (Unique i) _))  = do
    tyEnv <- get
    case IM.lookup i tyEnv of
        Just ty -> pure ty
        Nothing -> throwError $ UnfoundName l n
typeOf (Interp _ es) =
    traverse_ (tyAssert $ TyText ()) es $> TyText ()
typeOf (Lambda _ n ty e) =
    tyInsert n ty *>
    (TyFun () ty <$> typeOf e)
typeOf (Tuple _ es) = TyTuple () <$> traverse typeOf es
