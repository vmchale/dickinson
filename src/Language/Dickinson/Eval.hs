{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Eval ( EvalM
                               , EvalSt (..)
                               , HasEvalSt (..)
                               , addDecl
                               , loadDickinson
                               , evalDickinsonAsMain
                               , evalExpressionM
                               , evalWithGen
                               , evalIO
                               , findDecl
                               , findMain
                               ) where

import           Control.Monad.Except          (Except, MonadError, runExcept, throwError)
import           Control.Monad.State.Lazy      (MonadState, StateT, evalStateT, gets, modify)
import           Data.Foldable                 (toList, traverse_)
import qualified Data.IntMap                   as IM
import           Data.List.NonEmpty            (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Doc, Pretty (..), vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Ext
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                    (Lens', over)
import           System.Random                 (StdGen, newStdGen, randoms)

-- | The state during evaluation
data EvalSt a = EvalSt
    { probabilities :: [Double]
    -- map to expression
    , boundExpr     :: IM.IntMap (Expression Name a)
    , renameCtx     :: Renames
    -- TODO: map to uniques or an expression?
    , topLevel      :: M.Map T.Text Unique
    }

class HasEvalSt a where
    evalSt :: Lens' (a b) (EvalSt b)

instance HasEvalSt EvalSt where
    evalSt = id

prettyBound :: (Int, Expression Name a) -> Doc b
prettyBound (i, e) = pretty i <+> "←" <#*> (pretty e)

prettyTl :: (T.Text, Unique) -> Doc a
prettyTl (t, i) = pretty t <+> ":" <+> pretty i

instance Pretty (EvalSt a) where
    pretty (EvalSt _ b r t) =
        "bound expressions:" <#> vsep (prettyBound <$> IM.toList b)
            <#> pretty r
            <#> "top-level names:" <#> vsep (prettyTl <$> M.toList t)

instance HasRenames (EvalSt a) where
    rename f s = fmap (\x -> s { renameCtx = x }) (f (renameCtx s))

probabilitiesLens :: Lens' (EvalSt a) [Double]
probabilitiesLens f s = fmap (\x -> s { probabilities = x }) (f (probabilities s))

boundExprLens :: Lens' (EvalSt a) (IM.IntMap (Expression Name a))
boundExprLens f s = fmap (\x -> s { boundExpr = x }) (f (boundExpr s))

topLevelLens :: Lens' (EvalSt a) (M.Map T.Text Unique)
topLevelLens f s = fmap (\x -> s { topLevel = x }) (f (topLevel s))

-- TODO: thread generator state instead?
type EvalM a = StateT (EvalSt a) (Except (DickinsonError a))

evalIO :: UniqueCtx -> EvalM a x -> IO (Either (DickinsonError a) x)
evalIO rs me = (\g -> evalWithGen g rs me) <$> newStdGen

evalWithGen :: StdGen
            -> UniqueCtx -- ^ Threaded through
            -> EvalM a x
            -> Either (DickinsonError a) x
evalWithGen g u me = runExcept $ evalStateT me (EvalSt (randoms g) mempty (initRenames u) mempty)

-- TODO: temporary bindings
bindName :: (HasEvalSt s, MonadState (s a) m) => Name a -> Expression Name a -> m ()
bindName (Name _ (Unique u) _) e = modify (over (evalSt.boundExprLens) (IM.insert u e))

topLevelAdd :: (HasEvalSt s, MonadState (s a) m) => Name a -> m ()
topLevelAdd (Name (n :| []) u _) = modify (over (evalSt.topLevelLens) (M.insert n u))
topLevelAdd (Name{})             = error "Top-level names cannot be qualified"

deleteName :: (HasEvalSt s, MonadState (s a) m) => Name a -> m ()
deleteName (Name _ (Unique u) _) = modify (over (evalSt.boundExprLens) (IM.delete u))

lookupName :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Name a -> m (Expression Name a)
lookupName n@(Name _ (Unique u) l) = go =<< gets (IM.lookup u.boundExpr)
    where go Nothing  = throwError (UnfoundName l n)
          go (Just x) = renameExpressionM x

normalize :: (Foldable t, Functor t, Fractional a) => t a -> t a
normalize xs = {-# SCC "normalize" #-} (/tot) <$> xs
    where tot = sum xs

cdf :: (Num a) => NonEmpty a -> [a]
cdf = {-# SCC "cdf" #-} NE.drop 2 . NE.scanl (+) 0 . (0 <|)

pick :: MonadState (EvalSt a) m => NonEmpty (Double, Expression name a) -> m (Expression name a)
pick brs = {-# SCC "pick" #-} do
    threshold <- gets (head.probabilities)
    modify (over probabilitiesLens tail)
    let ds = cdf (normalize (fst <$> brs))
        es = toList (snd <$> brs)
    pure $ snd . head . dropWhile ((<= threshold) . fst) $ zip ds es

{-# SPECIALIZE findDecl :: T.Text -> EvalM a (Expression Name a) #-}
findDecl :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => T.Text -> m (Expression Name a)
findDecl t = do
    tops <- gets topLevel
    case M.lookup t tops of
        Just (Unique i) -> do { es <- gets boundExpr ; pure (es IM.! i) }
        Nothing         -> throwError (NoText t)

findMain :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => m (Expression Name a)
findMain = findDecl "main"

evalDickinsonAsMain :: Dickinson Name a -> EvalM a T.Text
evalDickinsonAsMain d =
    loadDickinson d *>
    (evalExpressionM =<< findMain)

{-# SPECIALIZE loadDickinson :: Dickinson Name a -> EvalM a () #-}
loadDickinson :: MonadState (EvalSt a) m => Dickinson Name a -> m ()
loadDickinson = traverse_ addDecl

addDecl :: MonadState (EvalSt a) m => Declaration Name a -> m ()
addDecl (Define _ n e) = bindName n e *> topLevelAdd n

{-# SPECIALIZE evalExpressionM :: Expression Name a -> EvalM a T.Text #-}
evalExpressionM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Expression Name a -> m T.Text
evalExpressionM (Literal _ t)  = pure t
evalExpressionM (StrChunk _ t) = pure t
evalExpressionM (Var _ n)      = evalExpressionM =<< lookupName n
evalExpressionM (Choice _ pes) = evalExpressionM =<< pick pes
evalExpressionM (Interp _ es)  = mconcat <$> traverse evalExpressionM es
evalExpressionM (Let _ bs e) = do
    traverse_ (uncurry bindName) bs
    evalExpressionM e <* traverse_ deleteName (fst <$> bs)
    -- FIXME: assumes global uniqueness in renaming
