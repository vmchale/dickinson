module Language.Dickinson.Check.Exhaustive ( checkExhaustive
                                           ) where

import           Control.Applicative                ((<|>))
import           Data.Foldable                      (toList)
import           Data.Foldable.Ext
import           Data.List                          (inits)
import           Data.Maybe                         (mapMaybe)
import           Language.Dickinson.Error
import           Language.Dickinson.Pattern.Useless
import           Language.Dickinson.Type

-- | Check that there are no useless pattern clauses and check that the pattern
-- matches are exhaustive
checkExhaustive :: [Declaration a] -> Maybe (DickinsonWarning a)
checkExhaustive = checkDeclsM

checkDeclsM :: [Declaration a] -> Maybe (DickinsonWarning a)
checkDeclsM ds =
    let pEnv = runPatternM $ patternEnvDecls ds in
        foldMapAlternative (checkDecl pEnv) ds

checkDecl :: PatternEnv -> Declaration a -> Maybe (DickinsonWarning a)
checkDecl _ TyDecl{}         = Nothing
checkDecl env (Define _ _ e) = checkExpr env e

isExhaustiveErr :: PatternEnv -> [Pattern a] -> a -> Maybe (DickinsonWarning a)
isExhaustiveErr env ps loc =
    if isExhaustive env ps
        then Nothing
        else Just $ InexhaustiveMatch loc

uselessErr :: PatternEnv -> [Pattern a] -> Pattern a -> Maybe (DickinsonWarning a)
uselessErr env ps p = {-# SCC "uselessErr" #-}
    if useful env ps p
        then Nothing
        else Just $ UselessPattern (patAnn p) p

foliate :: [a] -> [([a], a)]
foliate = mapMaybe split . inits
    where split []  = Nothing
          split [_] = Nothing
          split xs  = Just (init xs, last xs)

checkMatch :: PatternEnv -> [Pattern a] -> a -> Maybe (DickinsonWarning a)
checkMatch env ps loc = {-# SCC "checkMatch" #-}
    foldMapAlternative (uncurry (uselessErr env)) ({-# SCC "foliate" #-} foliate ps)
    <|> isExhaustiveErr env ps loc

checkExpr :: PatternEnv -> Expression a -> Maybe (DickinsonWarning a)
checkExpr _ Var{}                = Nothing
checkExpr _ Literal{}            = Nothing
checkExpr _ StrChunk{}           = Nothing
checkExpr _ Constructor{}        = Nothing
checkExpr _ BuiltinFn{}          = Nothing
checkExpr _ Random{}             = Nothing
checkExpr env (Flatten _ e)      = checkExpr env e
checkExpr env (Annot _ e _)      = checkExpr env e
checkExpr env (Lambda _ _ _ e)   = checkExpr env e
checkExpr env (Choice _ brs)     = foldMapAlternative (checkExpr env) (snd <$> brs)
checkExpr env (Let _ brs e)      = foldMapAlternative (checkExpr env) (snd <$> brs) <|> checkExpr env e
checkExpr env (Bind _ brs e)     = foldMapAlternative (checkExpr env) (snd <$> brs) <|> checkExpr env e
checkExpr env (Interp _ es)      = foldMapAlternative (checkExpr env) es
checkExpr env (MultiInterp _ es) = foldMapAlternative (checkExpr env) es
checkExpr env (Apply _ e e')     = checkExpr env e <|> checkExpr env e'
checkExpr env (Concat _ es)      = foldMapAlternative (checkExpr env) es
checkExpr env (Tuple _ es)       = foldMapAlternative (checkExpr env) es
checkExpr env (Match l e brs)    =
    checkExpr env e
        <|> checkMatch env (toList (fst <$> brs)) l <|> foldMapAlternative (checkExpr env) (snd <$> brs)
