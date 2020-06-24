module Language.Dickinson.DuplicateCheck ( checkDuplicates
                                         ) where

import           Control.Applicative      ((<|>))
import           Data.Foldable            (toList)
import           Data.Foldable.Ext        (foldMapAlternative)
import           Data.Function            (on)
import           Data.List                (groupBy, sortBy)
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T
import           Language.Dickinson.Error
import           Language.Dickinson.Type

checkNames :: [(a, T.Text)] -> Maybe (DickinsonWarning a)
checkNames ns = foldMapAlternative announce (groupBy ((==) `on` snd) $ sortBy (compare `on` snd) ns)
    where announce (_:(l, y):_) = Just $ DuplicateStr l y
          announce _            = Nothing

-- | Check that there are no duplicate names as the top-level
checkDuplicates :: Dickinson a -> Maybe (DickinsonWarning a)
checkDuplicates = foldMapAlternative checkDeclDuplicates

checkDeclDuplicates :: Declaration a -> Maybe (DickinsonWarning a)
checkDeclDuplicates (Define _ _ e) = checkExprDuplicates e
checkDeclDuplicates Import{}       = Nothing

extrText :: Expression a -> Maybe (a, T.Text)
extrText (Literal l t) = pure (l, t)
extrText _             = Nothing

collectText :: [(b, Expression a)] -> [(a, T.Text)]
collectText = mapMaybe (extrText . snd)

checkExprDuplicates :: Expression a -> Maybe (DickinsonWarning a)
checkExprDuplicates Var{}            = Nothing
checkExprDuplicates Literal{}        = Nothing
checkExprDuplicates StrChunk{}       = Nothing
checkExprDuplicates (Interp _ es)    = foldMapAlternative checkExprDuplicates es
checkExprDuplicates (Concat _ es)    = foldMapAlternative checkExprDuplicates es
checkExprDuplicates (Tuple _ es)     = foldMapAlternative checkExprDuplicates es
checkExprDuplicates (Apply _ e e')   = checkExprDuplicates e <|> checkExprDuplicates e'
checkExprDuplicates (Choice _ brs)   = checkNames (collectText $ toList brs)
checkExprDuplicates (Let _ brs es)   = foldMapAlternative checkExprDuplicates (snd <$> brs) <|> checkExprDuplicates es
checkExprDuplicates (Lambda _ _ _ e) = checkExprDuplicates e
checkExprDuplicates (Match _ e _ e') = checkExprDuplicates e <|> checkExprDuplicates e'
checkExprDuplicates (Flatten _ e)    = checkExprDuplicates e
checkExprDuplicates (Annot _ e _)    = checkExprDuplicates e
