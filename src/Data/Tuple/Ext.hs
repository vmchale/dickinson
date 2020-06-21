module Data.Tuple.Ext ( fst4
                      , frth
                      ) where

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

frth :: (a, b, c, d) -> d
frth (_, _, _, x) = x
