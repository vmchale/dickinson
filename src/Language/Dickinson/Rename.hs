module Language.Dickinson.Rename ( renameDickinson
                                 , RenameM
                                 ) where

import           Control.Monad           (forM_)
import           Control.Monad.State     (State, evalState, gets, modify)
import           Data.Bifunctor          (second)
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
        Just j  -> pure $ Name n (Unique j) l
        Nothing -> pure pre

insertM :: Unique -> RenameM a ()
insertM = modify . insertMod
    where insertMod :: Unique -> Renames -> Renames
          insertMod (Unique i) ~(m, rs) =
            if i `IM.member` rs
                then (m + 1, IM.insert i (m+1) rs)
                else (1 + max i m, rs)

deleteM :: Unique -> RenameM a ()
deleteM (Unique i) = modify (second (IM.delete i))
-- don't bother deleting max; probably won't run out

-- todo: clone function

renameDickinson :: Dickinson Name a -> Dickinson Name a
renameDickinson = id

renameExpressionM :: Expression Name a -> RenameM a (Expression Name a)
renameExpressionM e@Literal{} = pure e
renameExpressionM (Var p n)   = Var p <$> replaceVar n
renameExpressionM (Choice p branches) = Choice p <$> branches'
    where branches' =
            let ds = fmap fst branches
                in let es = fmap snd branches
                    in NE.zip ds <$> traverse renameExpressionM es
renameExpressionM (Concat p es) = Concat p <$> traverse renameExpressionM es
renameExpressionM (Let p ns e) = do
    newBinds <- traverse renameExpressionM (snd <$> ns)
    forM_ (fst <$> ns) $ \(Name _ u _) ->
        insertM u
    e' <- renameExpressionM e
    forM_ (fst <$> ns) $ \(Name _ u _) ->
        deleteM u
    pure $ Let p (NE.zip (fst <$> ns) (newBinds)) e'