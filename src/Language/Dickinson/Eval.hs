{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Eval ( EvalM
                               , addDecl
                               , loadDickinson
                               , evalDickinsonAsMain
                               , evalExpressionM
                               , evalWithGen
                               , evalIO
                               , findMain
                               ) where

import           Control.Monad.Except      (Except, MonadError, runExcept, throwError)
import           Control.Monad.State.Lazy  (StateT, evalStateT, gets, modify)
import           Data.Foldable             (toList, traverse_)
import qualified Data.IntMap               as IM
import           Data.List.NonEmpty        (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map                  as M
import           Data.Semigroup            (sconcat)
import qualified Data.Text                 as T
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                (Lens', over)
import           System.Random             (StdGen, newStdGen, randoms)

data EvalSt a = EvalSt
    { probabilities :: [Double]
    -- map to expression
    , boundExpr     :: IM.IntMap (Expression Name a)
    , renameCtx     :: Renames
    , topLevel      :: M.Map T.Text Unique
    }

instance HasRenames (EvalSt a) where
    rename f s = fmap (\x -> s { renameCtx = x }) (f (renameCtx s))

probabilitiesLens :: Lens' (EvalSt a) [Double]
probabilitiesLens f s = fmap (\x -> s { probabilities = x }) (f (probabilities s))

boundExprLens :: Lens' (EvalSt a) (IM.IntMap (Expression Name a))
boundExprLens f s = fmap (\x -> s { boundExpr = x }) (f (boundExpr s))

topLevelLens :: Lens' (EvalSt a) (M.Map T.Text Unique)
topLevelLens f s = fmap (\x -> s { topLevel = x }) (f (topLevel s))

-- TODO: thread generator state instead?
type EvalM name a = StateT (EvalSt a) (Except (DickinsonError name a))

evalIO :: UniqueCtx -> EvalM name a x -> IO (Either (DickinsonError name a) x)
evalIO rs me = (\g -> evalWithGen g rs me) <$> newStdGen

evalWithGen :: StdGen
            -> UniqueCtx -- ^ Threaded through
            -> EvalM name a x
            -> Either (DickinsonError name a) x
evalWithGen g u me = runExcept $ evalStateT me (EvalSt (randoms g) mempty (initRenames u) mempty)

-- TODO: temporary bindings
bindName :: Name a -> Expression Name a -> EvalM Name a ()
bindName (Name _ (Unique u) _) e = modify (over boundExprLens (IM.insert u e))

topLevelAdd :: Name a -> EvalM Name a ()
topLevelAdd (Name (n :| []) u _) = modify (over topLevelLens (M.insert n u))
topLevelAdd (Name{})             = error "Error message not yet implemented."

deleteName :: Name a -> EvalM Name a ()
deleteName (Name _ (Unique u) _) = modify (over boundExprLens (IM.delete u))

lookupName :: Name a -> EvalM Name a (Expression Name a)
lookupName n@(Name _ (Unique u) l) = go =<< gets (IM.lookup u.boundExpr)
    where go Nothing  = throwError (UnfoundName l n)
          go (Just x) = renameExpressionM x

normalize :: (Foldable t, Functor t, Fractional a) => t a -> t a
normalize xs = {-# SCC "normalize" #-} (/tot) <$> xs
    where tot = sum xs

cdf :: (Num a) => NonEmpty a -> [a]
cdf = {-# SCC "cdf" #-} NE.drop 2 . NE.scanl (+) 0 . (0 <|)

pick :: NonEmpty (Double, Expression name a) -> EvalM name a (Expression name a)
pick brs = {-# SCC "pick" #-} do
    threshold <- gets (head.probabilities)
    modify (over probabilitiesLens tail)
    let ds = cdf (normalize (fst <$> brs))
        es = toList (snd <$> brs)
    pure $ snd . head . dropWhile ((<= threshold) . fst) $ zip ds es

findMain :: EvalM name a (Expression Name a)
findMain = do
    tops <- gets topLevel
    case M.lookup "main" tops of
        Just (Unique i) -> do { es <- gets boundExpr ; pure (es IM.! i) }
        Nothing         -> throwError NoMain

evalDickinsonAsMain :: Dickinson Name a -> EvalM Name a T.Text
evalDickinsonAsMain d = do
    loadDickinson d
    e <- findMain
    evalExpressionM e

loadDickinson :: Dickinson Name a -> EvalM Name a ()
loadDickinson = traverse_ addDecl

addDecl :: Declaration Name a -> EvalM Name a ()
addDecl (Define _ n e) = bindName n e *> topLevelAdd n

evalExpressionM :: Expression Name a -> EvalM Name a T.Text
evalExpressionM (Literal _ t)  = pure t
evalExpressionM (Concat _ es)  = sconcat <$> traverse evalExpressionM es
evalExpressionM (Var _ n)      = evalExpressionM =<< lookupName n
evalExpressionM (Choice _ pes) = evalExpressionM =<< pick pes
evalExpressionM (Let _ bs e) = do
    traverse_ (uncurry bindName) bs
    evalExpressionM e <* traverse_ deleteName (fst <$> bs)
    -- FIXME: doesn't drop context!
