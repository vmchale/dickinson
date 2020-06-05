{-# LANGUAGE TupleSections #-}

module Language.Dickinson.Rename ( renameDickinson
                                 , renameExpressionM
                                 , RenameM
                                 , Renames (..)
                                 , HasRenames (..)
                                 ) where

import           Control.Monad           (void)
import           Control.Monad.State     (MonadState, State, evalState, gets,
                                          modify, state)
import           Data.Foldable           (traverse_)
import qualified Data.IntMap             as IM
import qualified Data.List.NonEmpty      as NE
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Lens.Micro              (Lens', over, set, (^.))
import           Lens.Micro.Extras       (view)
import           Lens.Micro.Mtl          (modifying, use)

-- type Renames = (Int, IM.IntMap Int)
data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }

boundLens :: Lens' Renames (IM.IntMap Int)
boundLens f s = fmap (\x -> s { bound = x}) (f (bound s))

class HasRenames a where
    rename :: Lens' a Renames

instance HasRenames Renames where
    rename = id

type RenameM a = State Renames

runRenameM :: RenameM a (f a) -> f a
runRenameM = flip evalState (Renames 0 mempty)

replaceVar :: (MonadState s m, HasRenames s) => Name a -> m (Name a)
replaceVar ~pre@(Name n (Unique i) l) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        -- for recursive lookups rewrites
        Just j  -> replaceVar $ Name n (Unique j) l
        Nothing -> pure pre

insertM :: (MonadState s m, HasRenames s) => Unique -> m Unique
insertM = state . insertGo
    where insertGo :: HasRenames s => Unique -> s -> (Unique, s)
          insertGo u st = let (u', r) = insertMod u (st^.rename) in (u', set rename r st)
          insertMod :: Unique -> Renames -> (Unique, Renames)
          insertMod (Unique i) (Renames m rs) =
                (Unique $ m+1, Renames (m+1) (IM.insert i (m+1) rs))

deleteM :: (MonadState s m, HasRenames s) => Unique -> m ()
deleteM (Unique i) = modifying (rename.boundLens) (IM.delete i)
-- don't bother deleting max; probably won't run out

renameDickinson :: Dickinson Name a -> Dickinson Name a
renameDickinson ds = runRenameM $ traverse renameDeclarationM ds

renameDeclarationM :: Declaration Name a -> RenameM a (Declaration Name a)
renameDeclarationM (Define p n@(Name _ u _) e) = do
    void $ insertM u
    Define p n <$> renameExpressionM e

withBinding :: (MonadState s m, HasRenames s) => (Name a, Expression Name a) -> m (Name a, Expression Name a)
withBinding (Name n u l, e) = do
    u' <- insertM u
    (Name n u' l ,) <$> renameExpressionM e

-- TODO: is this 'clone'?
renameExpressionM :: (MonadState s m, HasRenames s) => Expression Name a -> m (Expression Name a)
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
