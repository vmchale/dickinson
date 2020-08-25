module Data.List.Ext ( forAnyA
                     ) where

-- TODO: does this short-circuit appropriately with (strict) state monad?
anyA :: (Traversable t, Applicative f) => (a -> f Bool) -> t a -> f Bool
anyA p = fmap or . traverse p

forAnyA :: (Traversable t, Applicative f) => t a -> (a -> f Bool) -> f Bool
forAnyA = flip anyA
