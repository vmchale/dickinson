module Language.Dickinson.Name ( Name (..)
                               ) where

import qualified Data.Text as T

newtype Unique = Unique Word
    deriving (Eq, Ord)

data Name a = Name { name   :: T.Text
                   , unique :: !Unique
                   , loc    :: a
                   }

instance Eq (Name a) where
    (==) ~(Name _ u _) ~(Name _ u' _) = u == u'

instance Ord (Name a) where
    compare ~(Name _ u _) ~(Name _ u' _) = compare u u'
