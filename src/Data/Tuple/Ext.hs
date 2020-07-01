module Data.Tuple.Ext ( fst4
                      ) where

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x
