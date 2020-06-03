{-# LANGUAGE TupleSections #-}

module Language.Dickinson.Rename ( renameDickinson
                                 , RenameM
                                 ) where

import           Control.Monad           (void)
import           Control.Monad.State     (State, evalState, gets, modify, state)
import           Data.Bifunctor          (second)
import           Data.Foldable           (traverse_)
import qualified Data.IntMap             as IM
import qualified Data.List.NonEmpty      as NE
import           Language.Dickinson.Name
import           Language.Dickinson.Type

type Renames = (Int, IM.IntMap Int)

type RenameM a = State Renames

runRenameM :: RenameM a (f a) -> f a
runRenameM = flip evalState (0, mempty)

replaceVar :: Name a -> RenameM a (Name a)
replaceVar ~pre@(Name n (Unique i) l) = do
    rSt <- gets snd
    case IM.lookup i rSt of
        -- for recursive lookups rewrites
        Just j  -> replaceVar $ Name n (Unique j) l
        Nothing -> pure pre

insertM :: Unique -> RenameM a Unique
insertM = state . insertMod
    where insertMod :: Unique -> Renames -> (Unique, Renames)
          insertMod (Unique i) ~(m, rs) =
                (Unique $ m+1, (m + 1, IM.insert i (m+1) rs))

deleteM :: Unique -> RenameM a ()
deleteM (Unique i) = modify (second (IM.delete i))
-- don't bother deleting max; probably won't run out

renameDickinson :: Dickinson Name a -> Dickinson Name a
renameDickinson ds = runRenameM $ traverse renameDeclarationM ds

renameDeclarationM :: Declaration Name a -> RenameM a (Declaration Name a)
renameDeclarationM (Define p n@(Name _ u _) e) = do
    void $ insertM u
    Define p n <$> renameExpressionM e

withBinding :: (Name a, Expression Name a) -> RenameM a (Name a, Expression Name a)
withBinding (Name n u l, e) = do
    u' <- insertM u
    (Name n u' l ,) <$> renameExpressionM e

renameExpressionM :: Expression Name a -> RenameM a (Expression Name a)
renameExpressionM e@Literal{} = pure e
renameExpressionM (Var p n)   = Var p <$> replaceVar n
renameExpressionM (Choice p branches) = Choice p <$> branches'
    where branches' =
            let ds = fmap fst branches
                in let es = fmap snd branches
                    in NE.zip ds <$> traverse renameExpressionM es
renameExpressionM (Concat p es) = Concat p <$> traverse renameExpressionM es
renameExpressionM (Let p ls es) = do
    newBindings <- traverse withBinding ls
    res <- Let p newBindings <$> renameExpressionM es
    traverse_ deleteM extrUniques
    pure res

    where extrUniques = fmap (unique.fst) ls
