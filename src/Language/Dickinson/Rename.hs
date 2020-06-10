{-# LANGUAGE TupleSections #-}

module Language.Dickinson.Rename ( renameDickinson
                                 , renameExpressionM
                                 , initRenames
                                 , RenameM
                                 , Renames (..)
                                 , HasRenames (..)
                                 ) where

import           Control.Monad           (void)
import           Control.Monad.State     (MonadState, State, get, put, runState,
                                          state)
import           Data.Foldable           (traverse_)
import qualified Data.IntMap             as IM
import qualified Data.List.NonEmpty      as NE
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Lens.Micro              (Lens', set, (^.))
import           Lens.Micro.Mtl          (modifying, use, (%=), (.=))

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
    Define p n <$> renameExpressionM e

withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    postMax <- use (rename.maxLens)
    act <* do
        rename .= preSt
        rename.maxLens .= postMax

renameExpressionM :: (MonadState s m, HasRenames s) => Expression Name a -> m (Expression Name a)
renameExpressionM e@Literal{} = pure e
renameExpressionM (Var p n)   = Var p <$> replaceVar n
renameExpressionM (Choice p branches) = Choice p <$> branches'
    where branches' =
            let ds = fmap fst branches
                in let es = fmap snd branches
                    in NE.zip ds <$> traverse renameExpressionM es
renameExpressionM (Concat p es) = Concat p <$> traverse renameExpressionM es
