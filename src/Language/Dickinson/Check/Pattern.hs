module Language.Dickinson.Check.Pattern ( traversePattern
                                        , checkPatternDecl
                                        ) where

import           Control.Applicative      ((<|>))
import           Data.Foldable            (toList)
import           Data.Foldable.Ext        (foldMapAlternative)
import           Data.List                (group, sort)
import           Data.Maybe               (mapMaybe)
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

traversePattern :: Pattern a -> [Name a]
traversePattern (PatternVar _ n)    = [n]
traversePattern (PatternTuple _ ps) = traversePattern =<< toList ps
traversePattern Wildcard{}          = []
traversePattern PatternCons{}       = []
traversePattern (OrPattern _ ps)    = traversePattern =<< toList ps

checkPattern :: Pattern a -> Maybe (DickinsonError a)
checkPattern p = checkCoherent p <|> checkNames p

checkNames :: Pattern a -> Maybe (DickinsonError a)
checkNames p = foldMapAlternative announce (group $ sort (traversePattern p))
    where announce (_:y:_) = Just $ MultiBind (loc y) y p
          announce _       = Nothing

noVar :: Pattern a -> Bool
noVar PatternVar{}        = False
noVar (PatternTuple _ ps) = all noVar ps
noVar Wildcard{}          = True
noVar PatternCons{}       = True
noVar (OrPattern _ ps)    = all noVar ps

-- in theory I guess if an or-pattern bound a variable in all its leaves (and
-- all were the same type) I guess it would work
--
-- but this throws out any or-patterns containing wildcards or variables
checkCoherent :: Pattern a -> Maybe (DickinsonError a)
checkCoherent PatternVar{}                  = Nothing
checkCoherent (PatternTuple _ ps)           = foldMapAlternative checkCoherent ps
checkCoherent Wildcard{}                    = Nothing
checkCoherent PatternCons{}                 = Nothing
checkCoherent o@(OrPattern l _) | noVar o   = Nothing
                                | otherwise = Just $ SuspectPattern l o

checkPatternExpr :: Expression a -> Maybe (DickinsonError a)
checkPatternExpr Var{}              = Nothing
checkPatternExpr Literal{}          = Nothing
checkPatternExpr StrChunk{}         = Nothing
checkPatternExpr BuiltinFn{}        = Nothing
checkPatternExpr (Interp _ es)      = foldMapAlternative checkPatternExpr es
checkPatternExpr (MultiInterp _ es) = foldMapAlternative checkPatternExpr es
checkPatternExpr (Apply _ e e')     = checkPatternExpr e <|> checkPatternExpr e'
checkPatternExpr (Match _ e brs)    = foldMapAlternative (checkPattern . fst) brs <|> checkPatternExpr e <|> foldMapAlternative (checkPatternExpr . snd) brs
checkPatternExpr (Choice _ brs)     = foldMapAlternative (checkPatternExpr . snd) brs
checkPatternExpr (Concat _ es)      = foldMapAlternative checkPatternExpr es
checkPatternExpr (Tuple _ es)       = foldMapAlternative checkPatternExpr es
checkPatternExpr (Lambda _ _ _ e)   = checkPatternExpr e
checkPatternExpr (Flatten _ e)      = checkPatternExpr e
checkPatternExpr (Let _ bs e)       = foldMapAlternative (checkPatternExpr . snd) bs <|> checkPatternExpr e
checkPatternExpr (Bind _ bs e)      = foldMapAlternative (checkPatternExpr . snd) bs <|> checkPatternExpr e
checkPatternExpr (Annot _ e _)      = checkPatternExpr e
checkPatternExpr Constructor{}      = Nothing
checkPatternExpr Random{}           = Nothing

checkPatternDecl :: [Declaration a] -> Maybe (DickinsonError a)
checkPatternDecl ds =
    foldMapAlternative checkPatternExpr (mapMaybe defExprM ds)
