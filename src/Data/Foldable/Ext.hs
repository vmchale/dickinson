module Data.Foldable.Ext ( foldMapAlternative ) where

import           Control.Applicative (Alternative (empty, (<|>)))

foldMapAlternative :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
foldMapAlternative f = foldr (\x acc -> f x <|> acc) empty
