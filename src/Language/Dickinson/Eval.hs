{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Language.Dickinson.Eval ( EvalM
                               , evalExpressionM
                               , evalWithGen
                               , evalIO
                               ) where

import           Control.Monad.Except     (Except, ExceptT, runExcept,
                                           throwError)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.State.Lazy (StateT, evalStateT, gets, modify)
import           Data.Bifunctor           (first, second)
import           Data.Foldable            (toList)
import qualified Data.IntMap              as IM
import           Data.List.NonEmpty       (NonEmpty, (<|))
import qualified Data.List.NonEmpty       as NE
import           Data.Semigroup           (sconcat)
import qualified Data.Text                as T
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           System.Random            (StdGen, getStdGen, randoms)

-- map to expression
type EvalSt name a = ([Double], IM.IntMap (Expression name a))
-- thread infinite generator

-- TODO: thread generator state instead?
type EvalM name a = StateT (EvalSt name a) (Except (DickinsonError name a))

evalIO :: EvalM name a x -> IO (Either (DickinsonError name a) x)
evalIO me = flip evalWithGen me <$> getStdGen

evalWithGen :: StdGen -> EvalM name a x -> Either (DickinsonError name a) x
evalWithGen = (runExcept .) . (flip evalStateT . (, mempty) . randoms)

bindName :: Name a -> Expression Name a -> EvalM Name a ()
bindName (Name _ (Unique u) _) e = modify (second (IM.insert u e))

deleteName :: Name a -> EvalM Name a ()
deleteName (Name _ (Unique u) _) = modify (second (IM.delete u))

lookupName :: Name a -> EvalM Name a (Expression Name a)
lookupName n@(Name _ (Unique u) l) = go =<< gets (IM.lookup u.snd)
    where go Nothing  = throwError (UnfoundName l n)
          go (Just x) = pure x

normalize :: (Foldable t, Functor t, Fractional a) => t a -> t a
normalize xs = (/tot) <$> xs
    where tot = sum xs

cdf :: (Num a) => NonEmpty a -> [a]
cdf = NE.drop 2 . (NE.scanl (+) 0) . ((<|) 0)

pick :: NonEmpty (Double, Expression name a) -> EvalM name a (Expression name a)
pick brs = do
    threshold <- gets (head.fst)
    modify (first tail)
    let ds = cdf (normalize (fst <$> brs))
        es = toList (snd <$> brs)
    pure $ snd . head . dropWhile ((<= threshold) . fst) $ zip ds es

findMain :: Dickinson Name a -> EvalM Name a (Expression Name a)
findMain = getMain . filter (isMain.defName)
    where getMain (x:_) = pure $ defExpr x
          getMain []    = throwError NoMain
          -- TODO: complain if main is defined more than once

evalExpressionM :: Expression Name a -> EvalM Name a T.Text
evalExpressionM (Literal _ t)  = pure t
evalExpressionM (Concat _ es)  = sconcat <$> traverse evalExpressionM es
evalExpressionM (Var _ n)      = evalExpressionM =<< lookupName n
evalExpressionM (Choice _ pes) = evalExpressionM =<< pick pes
