module Data.List.Ext ( anyA
                     , allA
                     ) where

-- TODO: does this short-circuit appropriately with state monad?
anyA :: (Traversable t, Applicative f) => (a -> f Bool) -> t a -> f Bool
anyA p = fmap or . traverse p

-- TODO: does this short-circuit appropriately with state monad?
allA :: (Traversable t, Applicative f) => (a -> f Bool) -> t a -> f Bool
allA p = fmap and . traverse p
