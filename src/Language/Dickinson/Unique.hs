{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Dickinson.Unique ( Unique (..)
                                 , UniqueCtx
                                 ) where

import           Control.DeepSeq           (NFData)
import           Data.Binary               (Binary (..))
import           Data.Text.Prettyprint.Doc (Pretty)

-- | For interning identifiers.
newtype Unique = Unique { unUnique :: Int }
    deriving (Eq, Ord, Pretty, NFData, Binary)

type UniqueCtx = Int
