module Language.Dickinson.ScopeCheck ( checkScope
                                     ) where

import           Control.Applicative              (Alternative, (<|>))
import           Control.Monad.State              (StateT, evalStateT, get, modify)
import           Data.Foldable                    (asum, traverse_)
import qualified Data.IntSet                      as IS
import           Language.Dickinson.Check.Pattern
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique

-- TODO: renames and alex state
type CheckM = StateT IS.IntSet IO

runCheckM :: CheckM a -> IO a
runCheckM = flip evalStateT IS.empty

insertName :: Name a -> CheckM ()
insertName (Name _ (Unique i) _) = modify (IS.insert i)

deleteName :: Name a -> CheckM ()
deleteName (Name _ (Unique i) _) = modify (IS.delete i)

-- | Checks that there are not an identifiers that aren't in scope; needs to run
-- after the renamer
checkScope :: Dickinson a -> IO (Maybe (DickinsonError a))
checkScope = runCheckM . checkDickinson

checkDickinson :: Dickinson a -> CheckM (Maybe (DickinsonError a))
checkDickinson (Dickinson _ d) = traverse_ insDecl d *> mapSumM checkDecl d

insDecl :: Declaration a -> CheckM ()
insDecl (Define _ n _) = insertName n

checkDecl :: Declaration a -> CheckM (Maybe (DickinsonError a))
checkDecl (Define _ _ e) = checkExpr e

checkExpr :: Expression a -> CheckM (Maybe (DickinsonError a))
checkExpr Literal{}      = pure Nothing
checkExpr StrChunk{}     = pure Nothing
checkExpr (Apply _ e e') = (<|>) <$> checkExpr e <*> checkExpr e'
checkExpr (Interp _ es)  = mapSumM checkExpr es
checkExpr (Choice _ brs) = mapSumM checkExpr (snd <$> brs)
checkExpr (Concat _ es)  = mapSumM checkExpr es
checkExpr (Tuple _ es)   = mapSumM checkExpr es
checkExpr (Flatten _ e)  = checkExpr e
checkExpr (Annot _ e _)  = checkExpr e
checkExpr (Lambda _ n _ e) = do
    insertName n
    checkExpr e <* deleteName n
checkExpr (Var _ n@(Name _ (Unique i) l)) = do
    b <- get
    if i `IS.member` b
        then pure Nothing
        else pure $ Just (UnfoundName l n)
checkExpr (Let _ bs e) = do
    let ns = fst <$> bs
    traverse_ insertName ns
    (<|>) <$> checkExpr e <*> mapSumM checkExpr (snd <$> bs)
        <* traverse_ deleteName ns
checkExpr (Match _ e p e') =
    ((<|>) <$> checkExpr e) <*> do
        let ns = traversePattern p
        traverse_ insertName ns
        checkExpr e' <* traverse_ deleteName ns

mapSumM :: (Traversable t, Alternative f, Applicative m) => (a -> m (f b)) -> t a -> m (f b)
mapSumM = (fmap asum .) . traverse
