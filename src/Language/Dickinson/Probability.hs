{-# LANGUAGE TupleSections #-}
module Language.Dickinson.Probability ( weight
                                      ) where

import           Data.List.NonEmpty (NonEmpty)

weight :: NonEmpty a -> NonEmpty (Double, a)
weight es = (recip', ) <$> es
    where recip' = 1 / fromIntegral (length es)
