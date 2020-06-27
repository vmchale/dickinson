{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Rename ( renameDickinsonM
                                 , renameDeclarationM
                                 , renameExpressionM
                                 , initRenames
                                 , maxLens
                                 , replaceUnique
                                 , RenameM
                                 , Renames (..)
                                 , HasRenames (..)
                                 ) where

import           Control.Composition           (thread)
import           Control.Monad                 ((<=<))
import           Control.Monad.Ext             (zipWithM)
import           Control.Monad.State           (MonadState, State, runState)
import           Data.Bifunctor                (second)
import           Data.Binary                   (Binary)
import qualified Data.IntMap                   as IM
import qualified Data.List.NonEmpty            as NE
import           Data.Semigroup                (Semigroup (..))
import           Data.Text.Prettyprint.Doc     (Pretty (..), (<+>))
import           Data.Text.Prettyprint.Doc.Ext
import           GHC.Generics                  (Generic)
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                    (Lens')
import           Lens.Micro.Mtl                (modifying, use, (%=), (.=))

-- | Renamer state passed between various stages of compilation
data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }
    deriving (Generic, Binary)

instance Pretty Renames where
    pretty (Renames m b) = "max:" <+> pretty m <#> "renames:" <#*> prettyDumpBinds b

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
    mappend = (<>)

type RenameM a = State Renames

initRenames :: Renames
initRenames = Renames 0 mempty

runRenameM :: RenameM a x -> (x, UniqueCtx)
runRenameM x = second max_ (runState x initRenames)

-- Make sure you don't have cycles in the renames map!
replaceUnique :: (MonadState s m, HasRenames s) => Unique -> m Unique
replaceUnique u@(Unique i) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        Nothing -> pure u
        Just j  -> replaceUnique (Unique j)

replaceVar :: (MonadState s m, HasRenames s) => Name a -> m (Name a)
replaceVar (Name n u l) = {-# SCC "replaceVar" #-} do
    u' <- replaceUnique u
    pure $ Name n u' l

renameDickinsonM :: (MonadState s m, HasRenames s) => [Declaration a] -> m [Declaration a]
renameDickinsonM = traverse renameDeclarationM <=< traverse insDeclM

-- broadcast first...
insDeclM :: (MonadState s m, HasRenames s) => Declaration a -> m (Declaration a)
insDeclM (Define p n e) = do
    (n', modR) <- withName n
    modifying rename modR
    pure $ Define p n' e

renameDeclarationM :: (MonadState s m, HasRenames s) => Declaration a -> m (Declaration a)
renameDeclarationM (Define p n e) = do
    Define p n <$> renameExpressionM e

withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    -- idk
    postMax <- use (rename.maxLens)
    act <* (rename .= setMax (postMax + 1) preSt)

-- TODO: slow?
withName :: (HasRenames s, MonadState s m) => Name a -> m (Name a, Renames -> Renames)
withName (Name t (Unique i) l) = do
    m <- use (rename.maxLens)
    let newUniq = m+1
    rename.maxLens .= newUniq
    pure (Name t (Unique newUniq) l, mapBound (IM.insert i (m+1)))

mapBound :: (IM.IntMap Int -> IM.IntMap Int) -> Renames -> Renames
mapBound f (Renames m b) = Renames m (f b)

setMax :: Int -> Renames -> Renames
setMax i (Renames _ b) = Renames i b

renamePatternM :: (MonadState s m, HasRenames s) => Pattern a -> m (Renames -> Renames, Pattern a)
renamePatternM w@Wildcard{}        = pure (id, w)
renamePatternM (PatternTuple l ps) = do
    ps' <- traverse renamePatternM ps
    let modR = thread (fst <$> ps')
        ps'' = snd <$> ps'
    pure (modR, PatternTuple l ps'')
renamePatternM (PatternVar l n) = do
    (n', modR) <- withName n
    pure (modR, PatternVar l n')

renameExpressionM :: (MonadState s m, HasRenames s) => Expression a -> m (Expression a)
renameExpressionM e@Literal{} = pure e
renameExpressionM e@StrChunk{} = pure e
renameExpressionM (Var p n)   = Var p <$> replaceVar n
renameExpressionM (Choice p branches) = Choice p <$> branches'
    where branches' =
            let ds = fst <$> branches
                in let es = fmap snd branches
                    in NE.zip ds <$> traverse renameExpressionM es
renameExpressionM (Interp p es) = Interp p <$> traverse renameExpressionM es
renameExpressionM (Concat p es) = Concat p <$> traverse renameExpressionM es
renameExpressionM (Tuple p es)  = Tuple p <$> traverse renameExpressionM es
renameExpressionM (Apply p e e') = Apply p <$> renameExpressionM e <*> renameExpressionM e'
renameExpressionM (Lambda p n ty e) = do
    (n', modR) <- withName n
    Lambda p n' ty <$> withRenames modR (renameExpressionM e)
renameExpressionM (Match l e p e') = do
    preE <- renameExpressionM e
    (modP, p') <- renamePatternM p
    Match l preE p' <$> withRenames modP (renameExpressionM e')
renameExpressionM (Let p bs e) = do
    newBs <- traverse withName (fst <$> bs)
    let localRenames = snd <$> newBs
        newBinds = thread localRenames
        newNames = fst <$> newBs
        preNewBound = snd <$> bs
    newBound <-
        zipWithM (\r e' -> withRenames r (renameExpressionM e')) localRenames preNewBound
    withRenames newBinds $
        Let p (NE.zip newNames newBound) <$> renameExpressionM e
renameExpressionM (Flatten l e) =
    Flatten l <$> renameExpressionM e
renameExpressionM (Annot l e ty) =
    Annot l <$> renameExpressionM e <*> pure ty
