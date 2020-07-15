{-# LANGUAGE FlexibleContexts #-}

module Language.Dickinson.Pattern ( matchPattern
                                  ) where

import           Control.Monad.Except     (MonadError, throwError)
import           Data.List.NonEmpty       as NE
import           Language.Dickinson.Error
import           Language.Dickinson.Type

-- incoherency warning: or-patterns with wildcards & vars?
matches :: Pattern a -> Expression a -> Bool
matches Wildcard{} _                           = True
matches (PatternCons _ tn') (Constructor _ tn) = tn == tn'
matches PatternVar{} _                         = True
matches (PatternTuple _ ps) (Tuple _ es)       = and (NE.zipWith matches ps es) -- already check they're the same length during amalgamation
matches (OrPattern _ ps) e                     = any (`matches` e) ps
matches _ _                                    = False

matchPattern :: MonadError (DickinsonError a) m => a -> Expression a -> [(Pattern a, Expression a)] -> m (Pattern a, Expression a)
matchPattern l e (p:ps) | matches (fst p) e = pure p
                        | otherwise = matchPattern l e ps
matchPattern l e [] = throwError $ PatternFail l e
