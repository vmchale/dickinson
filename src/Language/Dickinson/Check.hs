module Language.Dickinson.Check ( checkMultiple
                                ) where

import           Control.Applicative      (Alternative (..))
import           Data.Foldable            (toList)
import           Data.Foldable.Ext        (foldMapAlternative)
import           Data.List                (group, sort)
import           Data.Maybe               (mapMaybe)
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

checkNames :: [Name a] -> Maybe (DickinsonWarning a)
checkNames ns = foldMapAlternative announce (group $ sort ns)
    where announce (_:y:_) = Just $ MultipleNames (loc y) y
          announce _       = Nothing

-- runs after the renamer
-- | Checks that there are not name clashes at the top level or within let
-- bindings.
checkMultiple :: [Declaration a] -> Maybe (DickinsonWarning a)
checkMultiple ds =
        checkNames (mapMaybe defNameM ds)
    <|> checkNames (mapMaybe tyDeclNameM ds)
    <|> foldMapAlternative checkMultipleExpr (mapMaybe defExprM ds)
    <|> checkNames (concatMap collectConstructors ds)
    where defNameM (Define _ n _) = Just n
          defNameM TyDecl{}       = Nothing
          defExprM (Define _ _ e) = Just e
          defExprM TyDecl{}       = Nothing
          tyDeclNameM Define{}       = Nothing
          tyDeclNameM (TyDecl _ n _) = Just n

collectConstructors :: Declaration a -> [TyName a]
collectConstructors Define{}        = []
collectConstructors (TyDecl _ _ cs) = toList cs

checkMultipleExpr :: Expression a -> Maybe (DickinsonWarning a)
checkMultipleExpr Var{}            = Nothing
checkMultipleExpr Literal{}        = Nothing
checkMultipleExpr StrChunk{}       = Nothing
checkMultipleExpr (Interp _ es)    = foldMapAlternative checkMultipleExpr es
checkMultipleExpr (Apply _ e e')   = checkMultipleExpr e <|> checkMultipleExpr e'
checkMultipleExpr (Match _ e _ e') = checkMultipleExpr e <|> checkMultipleExpr e'
checkMultipleExpr (Choice _ brs)   = foldMapAlternative (checkMultipleExpr . snd) brs
checkMultipleExpr (Concat _ es)    = foldMapAlternative checkMultipleExpr es
checkMultipleExpr (Tuple _ es)     = foldMapAlternative checkMultipleExpr es
checkMultipleExpr (Lambda _ _ _ e) = checkMultipleExpr e
checkMultipleExpr (Flatten _ e)    = checkMultipleExpr e
checkMultipleExpr (Let _ bs e)     =
        checkNames (toList $ fmap fst bs)
    <|> foldMapAlternative checkMultipleExpr (snd <$> bs)
    <|> checkMultipleExpr e
checkMultipleExpr (Annot _ e _)    = checkMultipleExpr e
checkMultipleExpr Constructor{}    = Nothing
