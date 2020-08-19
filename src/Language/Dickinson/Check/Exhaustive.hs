module Language.Dickinson.Check.Exhaustive ( checkExhaustive
                                           , foliate
                                           ) where

import           Control.Applicative                ((<|>))
import           Data.Foldable                      (toList)
import           Data.List                          (inits)
import           Data.Maybe                         (mapMaybe)
import           Language.Dickinson.Check.Common
import           Language.Dickinson.Error
import           Language.Dickinson.Pattern.Useless
import           Language.Dickinson.Type

checkExhaustive :: [Declaration a] -> Maybe (DickinsonWarning a)
checkExhaustive ds = runPatternM (checkDeclsM ds)

checkDeclsM :: [Declaration a] -> PatternM (Maybe (DickinsonWarning a))
checkDeclsM ds =
    patternEnvDecls ds *>
    mapSumM checkDeclM ds

checkDeclM :: Declaration a -> PatternM (Maybe (DickinsonWarning a))
checkDeclM TyDecl{}       = pure Nothing
checkDeclM (Define _ _ e) = checkExprM e

isExhaustiveM :: [Pattern a] -> a -> PatternM (Maybe (DickinsonWarning a))
isExhaustiveM ps loc = do
    e <- isExhaustive ps
    pure $ if e
        then Nothing
        else Just $ InexhaustiveMatch loc

uselessErr :: [Pattern a] -> Pattern a -> PatternM (Maybe (DickinsonWarning a))
uselessErr ps p = {-# SCC "uselessErr" #-} do
    e <- useful ps p
    pure $ if e
        then Nothing
        else Just $ UselessPattern (patAnn p) p

foliate :: [a] -> [([a], a)]
foliate = mapMaybe split . inits
    where split []  = Nothing
          split [_] = Nothing
          split xs  = Just (init xs, last xs)

checkMatch :: [Pattern a] -> a -> PatternM (Maybe (DickinsonWarning a))
checkMatch ps loc = {-# SCC "checkMatch" #-}
    (<|>)
        <$> mapSumM (uncurry uselessErr) ({-# SCC "foliate" #-} foliate ps)
        <*> isExhaustiveM ps loc

checkExprM :: Expression a -> PatternM (Maybe (DickinsonWarning a))
checkExprM Var{}              = pure Nothing
checkExprM Literal{}          = pure Nothing
checkExprM StrChunk{}         = pure Nothing
checkExprM Constructor{}      = pure Nothing
checkExprM BuiltinFn{}        = pure Nothing
checkExprM (Flatten _ e)      = checkExprM e
checkExprM (Annot _ e _)      = checkExprM e
checkExprM (Lambda _ _ _ e)   = checkExprM e
checkExprM (Choice _ brs)     = mapSumM checkExprM (snd <$> brs)
checkExprM (Let _ brs e)      = (<|>) <$> mapSumM checkExprM (snd <$> brs) <*> checkExprM e
checkExprM (Interp _ es)      = mapSumM checkExprM es
checkExprM (MultiInterp _ es) = mapSumM checkExprM es
checkExprM (Apply _ e e')     = (<|>) <$> checkExprM e <*> checkExprM e'
checkExprM (Concat _ es)      = mapSumM checkExprM es
checkExprM (Tuple _ es)       = mapSumM checkExprM es
checkExprM (Match l e brs)    =
    (<|>)
        <$> checkExprM e
        <*> ((<|>) <$> checkMatch (toList (fst <$> brs)) l <*> mapSumM checkExprM (snd <$> brs))
