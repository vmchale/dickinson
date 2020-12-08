{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Dickinson.Unique ( Unique (..)
                                 , UniqueCtx
                                 , dummyUnique
                                 ) where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary (..))
import           Data.Data       (Data)
import           Prettyprinter   (Pretty)
import           Test.QuickCheck (Arbitrary)

-- | For interning identifiers.
newtype Unique = Unique { unUnique :: Int }
    deriving (Eq, Ord, Pretty, NFData, Binary, Show, Data, Arbitrary)

-- | Dummy unique for sake of testing
dummyUnique :: Unique
dummyUnique = Unique 0

type UniqueCtx = Int
