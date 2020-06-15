module Language.Dickinson.Check ( checkMultiple
                                ) where

import           Control.Applicative      (Alternative (..))
import           Data.Foldable            (asum)
import           Data.Foldable            (toList)
import           Data.List                (group, sort)
import           Data.Maybe               (mapMaybe)
import           Language.Dickinson.Error
import           Language.Dickinson.Name
import           Language.Dickinson.Type

checkNames :: [Name a] -> Maybe (DickinsonError Name a)
checkNames ns = foldMapAlternative announce (group $ sort ns)
    where announce (_:y:_) = Just $ MultipleNames (loc y) y
          announce _       = Nothing

-- runs after the parser
checkMultiple :: Dickinson Name a -> Maybe (DickinsonError Name a)
checkMultiple ds =
        checkNames (mapMaybe extrName ds)
    <|> foldMapAlternative checkMultipleExpr (mapMaybe extrExpr ds)
    where extrName (Define _ n _) = Just n
          extrName Import{}       = Nothing
          extrExpr (Define _ _ e) = Just e
          extrExpr Import{}       = Nothing

checkMultipleExpr :: Expression Name a -> Maybe (DickinsonError Name a)
checkMultipleExpr Var{}          = Nothing
checkMultipleExpr Literal{}      = Nothing
checkMultipleExpr StrChunk{}     = Nothing
checkMultipleExpr (Choice _ brs) = foldMapAlternative (checkMultipleExpr . snd) brs
checkMultipleExpr (Let _ bs e)   =
        checkNames (toList $ fmap fst bs)
    <|> foldMapAlternative checkMultipleExpr (snd <$> bs)
    <|> checkMultipleExpr e

foldMapAlternative :: (Traversable t, Alternative f) => (a -> f b) -> t a -> f b
foldMapAlternative f xs = asum (f <$> xs)
