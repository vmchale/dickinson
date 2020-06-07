{-# LANGUAGE TupleSections #-}

module Language.Dickinson.Rename ( renameDickinson
                                 , renameExpressionM
                                 , initRenames
                                 , RenameM
                                 , Renames (..)
                                 , HasRenames (..)
                                 ) where

import           Control.Monad           (void)
import           Control.Monad.State     (MonadState, State, runState, state)
import           Data.Foldable           (traverse_)
import qualified Data.IntMap             as IM
import qualified Data.List.NonEmpty      as NE
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Lens.Micro              (Lens', set, (^.))
import           Lens.Micro.Mtl          (modifying, use)

data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }

boundLens :: Lens' Renames (IM.IntMap Int)
boundLens f s = fmap (\x -> s { bound = x}) (f (bound s))

class HasRenames a where
    rename :: Lens' a Renames

instance HasRenames Renames where
    rename = id

type RenameM a = State Renames

initRenames :: Renames
initRenames = Renames (maxBound `div` 2) mempty -- FIXME: this is sloppy

runRenameM :: RenameM a x -> (x, Renames)
runRenameM = flip runState initRenames

replaceVar :: (MonadState s m, HasRenames s) => Name a -> m (Name a)
replaceVar ~pre@(Name n (Unique i) l) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        -- for recursive lookups rewrites
        -- FIXME: we have cycles?
        Just j  -> replaceVar $ Name n (Unique j) l
        Nothing -> pure pre

insertM :: (MonadState s m, HasRenames s) => Unique -> m (Unique, Unique)
insertM = state . insertGo
    where insertGo :: HasRenames s => Unique -> s -> ((Unique, Unique), s)
          insertGo u st = let (u', r) = insertMod u (st^.rename) in (u', set rename r st)
          insertMod :: Unique -> Renames -> ((Unique, Unique), Renames)
          insertMod (Unique i) (Renames m rs) =
                case IM.lookup i rs of
                    Just j -> undefined -- (Unique $ m+1, Renames (m+1) (IM.insert i (m+1) rs))
                    Nothing -> ((Unique i, Unique $ m+1), Renames (m+1) (IM.insert i (m+1) rs))
                    -- TODO: also return what to delete at the end?

deleteM :: (MonadState s m, HasRenames s) => Unique -> m ()
deleteM (Unique i) = modifying (rename.boundLens) (IM.delete i)
-- don't bother deleting max; probably won't run out

renameDickinson :: Dickinson Name a -> (Dickinson Name a, Renames)
renameDickinson ds = runRenameM $ traverse renameDeclarationM ds

renameDeclarationM :: (MonadState s m, HasRenames s) => Declaration Name a -> m (Declaration Name a)
renameDeclarationM (Define p n@(Name _ u _) e) = do
    void $ insertM u
    Define p n <$> renameExpressionM e

withBinding :: (MonadState s m, HasRenames s) => (Name a, Expression Name a) -> m (Unique, Name a, Expression Name a)
withBinding (Name n u l, e) = do
    (j, u') <- insertM u
    (j, Name n u' l ,) <$> renameExpressionM e

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
    res <- Let p (to2 <$> newBindings) <$> renameExpressionM es
    traverse_ deleteM (extrUniques newBindings)
    -- inserts are too zealous? can bind to e.g. '2' twice...
    -- and then delete it prematurely?
    pure res

    where extrUniques = fmap fst3
          to2 (_, x, y) = (x, y)
          fst3 (x, _, _) = x
