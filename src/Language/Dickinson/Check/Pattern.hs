module Language.Dickinson.Check.Pattern ( checkNames
                                        , traversePattern
                                        ) where

import           Data.Foldable.Ext        (foldMapAlternative)
import           Data.List                (group, sort)
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

traversePattern :: Pattern a -> [Name a]
traversePattern (PatternVar _ n)    = [n]
traversePattern (PatternTuple _ ps) = traversePattern =<< ps
traversePattern Wildcard{}          = []

checkNames :: Pattern a -> Maybe (DickinsonError a)
checkNames p = foldMapAlternative announce (group $ sort (traversePattern p))
    where announce (_:y:_) = Just $ MultiBind (loc y) y p
          announce _       = Nothing
