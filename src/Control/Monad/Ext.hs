module Control.Monad.Ext ( zipWithM
                         ) where

import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE

zipWithM :: (Applicative m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithM f xs ys =  sequenceA (NE.zipWith f xs ys)
