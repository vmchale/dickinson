{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Dickinson.Unique ( Unique (..)
                                 ) where

import           Control.DeepSeq           (NFData)
import           Data.Binary               (Binary (..))
import           Data.Text.Prettyprint.Doc (Pretty)

newtype Unique = Unique { unUnique :: Int }
    deriving (Eq, Ord, Pretty, NFData, Binary)
