{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Check.Scope ( checkScope
                                      , checkScopeExprWith
                                      , checkScopeDeclWith
                                      ) where

import           Control.Applicative              ((<|>))
import           Control.Monad.Except             (MonadError)
import           Control.Monad.State.Strict       (State, evalState, get, modify)
import           Data.Foldable                    (traverse_)
import qualified Data.IntSet                      as IS
import           Data.List.NonEmpty               (NonEmpty)
import           Language.Dickinson.Check.Common
import           Language.Dickinson.Check.Pattern
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique

type CheckM = State IS.IntSet

runCheckM :: CheckM a -> a
runCheckM = flip evalState IS.empty

insertName :: Name a -> CheckM ()
insertName (Name _ (Unique i) _) = modify (IS.insert i)

deleteName :: Name a -> CheckM ()
deleteName (Name _ (Unique i) _) = modify (IS.delete i)

-- | Checks that there are not any identifiers that aren't in scope; needs to run
-- after the renamer
checkScope :: [Declaration a] -> Maybe (DickinsonError a)
checkScope = runCheckM . checkDickinson

checkScopeExprWith :: MonadError (DickinsonError a) m => IS.IntSet -> Expression a -> m ()
checkScopeExprWith is = maybeThrow . flip evalState is . checkExpr

checkScopeDeclWith :: MonadError (DickinsonError a) m => IS.IntSet -> Declaration a -> m ()
checkScopeDeclWith is = maybeThrow . flip evalState is . checkDecl

checkDickinson :: [Declaration a] -> CheckM (Maybe (DickinsonError a))
checkDickinson d = traverse_ insDecl d *> mapSumM checkDecl d

insDecl :: Declaration a -> CheckM ()
insDecl (Define _ n _)   = insertName n
insDecl (TyDecl _ n tys) = insertName n *> traverse_ insertName tys

checkDecl :: Declaration a -> CheckM (Maybe (DickinsonError a))
checkDecl (Define _ _ e) = checkExpr e
checkDecl TyDecl{}       = pure Nothing

checkType :: DickinsonTy a -> CheckM (Maybe (DickinsonError a))
checkType TyText{}         = pure Nothing
checkType (TyFun _ ty ty') = (<|>) <$> checkType ty <*> checkType ty'
checkType (TyTuple _ tys)  = mapSumM checkType tys
checkType (TyNamed l n@(Name _ (Unique k) _)) = do
    b <- get
    if k `IS.member` b
        then pure Nothing
        else pure $ Just (UnfoundType l n)

checkExpr :: Expression a -> CheckM (Maybe (DickinsonError a))
checkExpr BuiltinFn{}    = pure Nothing
checkExpr Literal{}      = pure Nothing
checkExpr StrChunk{}     = pure Nothing
checkExpr (Apply _ e e') = (<|>) <$> checkExpr e <*> checkExpr e'
checkExpr (Interp _ es)  = mapSumM checkExpr es
checkExpr (MultiInterp _ es)  = mapSumM checkExpr es
checkExpr (Choice _ brs) = mapSumM checkExpr (snd <$> brs)
checkExpr (Concat _ es)  = mapSumM checkExpr es
checkExpr (Tuple _ es)   = mapSumM checkExpr es
checkExpr (Flatten _ e)  = checkExpr e
checkExpr (Annot _ e ty) = (<|>) <$> checkExpr e <*> checkType ty
checkExpr (Lambda _ n _ e) = do
    insertName n
    checkExpr e <* deleteName n
checkExpr (Constructor _ n@(Name _ (Unique i) l)) = do
    b <- get
    if i `IS.member` b
        then pure Nothing
        else pure $ Just (UnfoundConstructor l n)
checkExpr (Var _ n@(Name _ (Unique i) l)) = do
    b <- get
    if i `IS.member` b
        then pure Nothing
        else pure $ Just (UnfoundName l n)
checkExpr (Let _ bs e) = checkLet bs e
checkExpr (Bind _ bs e) = checkLet bs e
checkExpr (Match _ e brs) =
    ((<|>) <$> checkExpr e) <*> mapSumM checkPair brs
checkExpr (Random l n) =
    checkType (TyNamed l n)

-- @:let@s and @:bind@s are the same here
checkLet :: NonEmpty (Name a, Expression a)
         -> Expression a
         -> CheckM (Maybe (DickinsonError a))
checkLet bs e = do
    let ns = fst <$> bs
    traverse_ insertName ns
    (<|>) <$> checkExpr e <*> mapSumM checkExpr (snd <$> bs)
        <* traverse_ deleteName ns

checkPair :: (Pattern a, Expression a) -> CheckM (Maybe (DickinsonError a))
checkPair (p, e) = do
    let ns = traversePattern p
    traverse_ insertName ns
    checkExpr e <* traverse_ deleteName ns
