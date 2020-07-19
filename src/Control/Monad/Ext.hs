module Control.Monad.Ext ( zipWithM
                         , zipWithM_
                         ) where

import           Data.Foldable      (sequenceA_)
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE

zipWithM :: (Applicative m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithM f xs ys = sequenceA (NE.zipWith f xs ys)

zipWithM_ :: (Applicative m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m ()
zipWithM_ f xs ys = sequenceA_ (NE.zipWith f xs ys)
