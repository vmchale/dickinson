{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Language.Dickinson.Rename ( renameDickinson
                                 , renameDeclarationsM
                                 , renameDeclarationM
                                 , renameExpressionM
                                 , initRenames
                                 , maxLens
                                 , boundLens
                                 , replaceUnique
                                 , RenameM
                                 , Renames (..)
                                 , HasRenames (..)
                                 ) where

import           Control.Composition           (thread)
import           Control.Monad                 (forM, (<=<))
import           Control.Monad.State           (MonadState, State, runState)
import           Data.Bifunctor                (second)
import           Data.Binary                   (Binary)
import qualified Data.IntMap                   as IM
import qualified Data.List.NonEmpty            as NE
import           Data.Text.Prettyprint.Doc.Ext
import           GHC.Generics                  (Generic)
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                    (Lens')
import           Lens.Micro.Mtl                (modifying, use, (%=), (.=))
import           Prettyprinter                 (Pretty (..), (<+>))

-- | Renamer state passed between various stages of compilation
data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }
    deriving (Generic, Binary)

instance Pretty Renames where
    pretty (Renames m b) = "max:" <+> pretty m <#> "renames:" <#*> prettyDumpBinds b

boundLens :: Lens' Renames (IM.IntMap Int)
boundLens f s = fmap (\x -> s { bound = x }) (f (bound s))

maxLens :: Lens' Renames Int
maxLens f s = fmap (\x -> s { max_ = x }) (f (max_ s))

-- | @since 0.1.1.0
class HasRenames a where
    rename :: Lens' a Renames

instance HasRenames Renames where
    rename = id

type RenameM = State Renames

initRenames :: Renames
initRenames = Renames 0 mempty

runRenameM :: Int -> RenameM x -> (x, UniqueCtx)
runRenameM i x = second max_ (runState x (Renames i mempty))

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

-- exported so we can test it alone
renameDickinson :: Int -> Dickinson a -> (Dickinson a, Int)
renameDickinson m ds = runRenameM m $ renameDickinsonM ds

-- | The renamer ensures global uniqueness and is used during evaluation to
-- clone expressions with bound variables.
renameDickinsonM :: (MonadState s m, HasRenames s) => Dickinson a -> m (Dickinson a)
renameDickinsonM (Dickinson i d) = Dickinson i <$> renameDeclarationsM d

renameDeclarationsM :: (MonadState s m, HasRenames s) => [Declaration a] -> m [Declaration a]
renameDeclarationsM = traverse renameDeclarationM <=< traverse insDeclM

-- broadcast first... This allows definitions to be declared in any order.
insDeclM :: (MonadState s m, HasRenames s) => Declaration a -> m (Declaration a)
insDeclM (Define p n e) = do
    (n', modR) <- withName n
    modifying rename modR
    pure $ Define p n' e
insDeclM d@TyDecl{} = pure d -- TODO: decide on spec for scoping. (two type decls should be illegal)

renameDeclarationM :: (MonadState s m, HasRenames s) => Declaration a -> m (Declaration a)
renameDeclarationM (Define p n e) =
    Define p n <$> renameExpressionM e
renameDeclarationM d@TyDecl{} = pure d

-- allows us to work with a temporary change to the renamer state, tracking the
-- max sensibly
withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    res <- act
    postMax <- use (rename.maxLens)
    rename .= setMax postMax preSt
    pure res

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
renamePatternM c@PatternCons{}     = pure (id, c) -- TODO: correct?
renamePatternM (OrPattern l ps) = do
    ps' <- traverse renamePatternM ps
    let modR = thread (fst <$> ps')
        ps'' = snd <$> ps'
    pure (modR, OrPattern l ps'')

-- | @since 0.1.1.0
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
renameExpressionM (MultiInterp p es) = MultiInterp p <$> traverse renameExpressionM es
renameExpressionM (Concat p es) = Concat p <$> traverse renameExpressionM es
renameExpressionM (Tuple p es)  = Tuple p <$> traverse renameExpressionM es
renameExpressionM (Apply p e e') = Apply p <$> renameExpressionM e <*> renameExpressionM e'
renameExpressionM (Lambda p n ty e) = do
    (n', modR) <- withName n
    Lambda p n' ty <$> withRenames modR (renameExpressionM e)
renameExpressionM (Match l e brs) = do
    preE <- renameExpressionM e
    brs' <- forM brs $ \(p, e') -> do
        (modP, p') <- renamePatternM p
        (p' ,) <$> withRenames modP (renameExpressionM e')
    pure $ Match l preE brs'
renameExpressionM (Bind p bs e) = renameLet Bind p bs e
renameExpressionM (Let p bs e) = renameLet Let p bs e
renameExpressionM (Flatten l e) =
    Flatten l <$> renameExpressionM e
renameExpressionM (Annot l e ty) =
    Annot l <$> renameExpressionM e <*> pure ty
renameExpressionM c@Constructor{} = pure c
renameExpressionM c@BuiltinFn{} = pure c
renameExpressionM c@Random{} = pure c

-- since bind/let are the same at this stage
renameLet :: (MonadState s m, HasRenames s)
          => (a -> NE.NonEmpty (Name a, Expression a) -> Expression a -> Expression a)
          -> a
          -> NE.NonEmpty (Name a, Expression a)
          -> Expression a
          -> m (Expression a)
renameLet constructor p bs e = do
    newBs <- traverse (withName.fst) bs
    let (newNames, localRenames) = NE.unzip newBs
        newBinds = thread localRenames
    newBound <-
        traverse (renameExpressionM.snd) bs
    withRenames newBinds $
        constructor p (NE.zip newNames newBound) <$> renameExpressionM e
