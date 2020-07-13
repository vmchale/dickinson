{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Pattern ( matchPattern
                                  ) where

import           Control.Monad.Except     (MonadError, throwError)
import           Data.List.NonEmpty       as NE
import           Language.Dickinson.Error
import           Language.Dickinson.Type

matches :: Expression a -> Pattern a -> Bool
matches _ Wildcard{}                           = True
matches (Constructor _ tn) (PatternCons _ tn') = tn == tn'
matches _ PatternVar{}                         = True
matches (Tuple _ es) (PatternTuple _ ps)       = and (NE.zipWith matches es ps) -- already check they're the same length during amalgamation
matches _ _                                    = False

matchPattern :: MonadError (DickinsonError a) m => a -> Expression a -> [(Pattern a, Expression a)] -> m (Pattern a, Expression a)
matchPattern l e (p:ps) | matches e (fst p) = pure p
                        | otherwise = matchPattern l e ps
matchPattern l e [] = throwError $ PatternFail l e
