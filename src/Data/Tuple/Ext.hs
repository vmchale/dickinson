module Data.Tuple.Ext ( fst3
                      ) where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
