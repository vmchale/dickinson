module Language.Dickinson.Check ( checkMultiple
                                ) where

import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

-- runs after the parser
checkMultiple :: Dickinson Name a -> Maybe (DickinsonError Name a)
checkMultiple _ = Nothing

checkMultipleExpr :: Expression name a -> Maybe (DickinsonError Name a)
checkMultipleExpr Var{}     = Nothing
checkMultipleExpr Literal{} = Nothing
