module Data.List.Ext ( anyA
                     , allA
                     , forAnyA
                     ) where

-- TODO: does this short-circuit appropriately with state monad?
anyA :: (Traversable t, Applicative f) => (a -> f Bool) -> t a -> f Bool
anyA p = fmap or . traverse p

-- TODO: does this short-circuit appropriately with state monad?
allA :: (Traversable t, Applicative f) => (a -> f Bool) -> t a -> f Bool
allA p = fmap and . traverse p

forAnyA :: (Traversable t, Applicative f) => t a -> (a -> f Bool) -> f Bool
forAnyA = flip anyA
