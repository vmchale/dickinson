{-# LANGUAGE TupleSections #-}

module Language.Dickinson.Rename ( renameDickinson
                                 , renameExpressionM
                                 , initRenames
                                 , RenameM
                                 , Renames (..)
                                 , HasRenames (..)
                                 ) where

import           Control.Composition     (thread)
import           Control.Monad           (void)
import           Control.Monad.Ext       (zipWithM)
import           Control.Monad.State     (MonadState, State, get, put, runState,
                                          state)
import           Data.Foldable           (traverse_)
import qualified Data.IntMap             as IM
import qualified Data.List.NonEmpty      as NE
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Lens.Micro              (Lens', over, set, (^.))
import           Lens.Micro.Mtl          (modifying, use, (%=), (+=), (.=))

data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }

boundLens :: Lens' Renames (IM.IntMap Int)
boundLens f s = fmap (\x -> s { bound = x }) (f (bound s))

maxLens :: Lens' Renames Int
maxLens f s = fmap (\x -> s { max_ = x }) (f (max_ s))

class HasRenames a where
    rename :: Lens' a Renames

instance HasRenames Renames where
    rename = id

instance Semigroup Renames where
    (<>) (Renames m1 b1) (Renames m2 b2) = Renames (max m1 m2) (b1 <> b2)

instance Monoid Renames where
    mempty = Renames 0 mempty

type RenameM a = State Renames

initRenames :: Renames
initRenames = Renames (maxBound `div` 2) mempty -- FIXME: this is sloppy

runRenameM :: RenameM a x -> (x, Renames)
runRenameM = flip runState initRenames

-- Make sure you don't have cycles in the renames map!
replaceVar :: (MonadState s m, HasRenames s) => Name a -> m (Name a)
replaceVar ~pre@(Name n (Unique i) l) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        Just j  -> replaceVar $ Name n (Unique j) l
        Nothing -> pure pre

renameDickinson :: Dickinson Name a -> (Dickinson Name a, Renames)
renameDickinson ds = runRenameM $ traverse renameDeclarationM ds

renameDeclarationM :: (MonadState s m, HasRenames s) => Declaration Name a -> m (Declaration Name a)
renameDeclarationM (Define p n@(Name _ u _) e) = do
    -- FIXME: broadcast unique?
    Define p n <$> renameExpressionM e

withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    -- idk
    rename.maxLens += 1
    postMax <- use (rename.maxLens)
    act <* do
        rename .= preSt
        rename.maxLens .= postMax

withName :: (HasRenames s, MonadState s m) => Name a -> m (Name a, Renames -> Renames)
withName (Name t (Unique i) l) = do
    m <- use (rename.maxLens)
    let newUniq = m+1
    rename.maxLens .= newUniq
    pure (Name t (Unique newUniq) l, over boundLens (IM.insert i (m+1)))

renameExpressionM :: (MonadState s m, HasRenames s) => Expression Name a -> m (Expression Name a)
renameExpressionM e@Literal{} = pure e
renameExpressionM (Var p n)   = Var p <$> replaceVar n
renameExpressionM (Choice p branches) = Choice p <$> branches'
    where branches' =
            let ds = fst <$> branches
                in let es = fmap snd branches
                    in NE.zip ds <$> traverse renameExpressionM es
renameExpressionM (Concat p es) = Concat p <$> traverse renameExpressionM es
renameExpressionM (Let p bs e) = do
    newBs <- traverse withName (fst <$> bs)
    let localRenames = snd <$> newBs
        newBinds = thread localRenames
        newNames = fst <$> newBs
        preNewBound = snd <$> bs
    newBound <- zipWithM (\r e -> withRenames r (renameExpressionM e)) localRenames preNewBound
    withRenames newBinds $
        Let p (NE.zip newNames newBound) <$> renameExpressionM e
