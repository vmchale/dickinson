module Language.Dickinson.ScopeCheck ( checkScope
                                     ) where

import           Control.Monad.State       (State, gets)
import qualified Data.IntSet               as IS
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique

newtype CheckSt = CheckSt
    { bound :: IS.IntSet }

type CheckM = State CheckSt

checkScope :: Dickinson Name a -> Maybe (DickinsonError Name a)
checkScope _ = Nothing

checkDickinson :: Dickinson Name a -> CheckM (Maybe (DickinsonError Name a))
checkDickinson _ = pure Nothing

checkExpr :: Expression Name a -> CheckM (Maybe (DickinsonError Name a))
checkExpr Literal{}  = pure Nothing
checkExpr StrChunk{} = pure Nothing
checkExpr (Var _ n@(Name _ (Unique i) l)) = do
    b <- gets bound
    if i `IS.member` b
        then pure Nothing
        else pure $ Just (UnfoundName l n)
