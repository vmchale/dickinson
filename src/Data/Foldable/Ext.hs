module Data.Foldable.Ext ( foldMapAlternative ) where

import           Control.Applicative (Alternative)
import           Data.Foldable       (asum)

foldMapAlternative :: (Traversable t, Alternative f) => (a -> f b) -> t a -> f b
foldMapAlternative f xs = asum (f <$> xs)
