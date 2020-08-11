module Language.Dickinson.Check.Common ( mapSumM
                                       ) where

import           Control.Applicative (Alternative)
import           Data.Foldable       (asum)

mapSumM :: (Traversable t, Alternative f, Applicative m) => (a -> m (f b)) -> t a -> m (f b)
mapSumM = (fmap asum .) . traverse
