{-# LANGUAGE DeriveFunctor #-}

module Language.Dickinson.Name ( Name (..)
                               , Unique (..)
                               , NameEnv
                               ) where

import qualified Data.IntMap               as IM
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Pretty (pretty))

newtype Unique = Unique Int
    deriving (Eq, Ord)

data Name a = Name { name   :: T.Text
                   , unique :: !Unique
                   , loc    :: a
                   } deriving (Functor)

instance Eq (Name a) where
    (==) ~(Name _ u _) ~(Name _ u' _) = u == u'

instance Ord (Name a) where
    compare ~(Name _ u _) ~(Name _ u' _) = compare u u'

instance Pretty (Name a) where
    pretty (Name t _ _) = pretty t

type NameEnv a = IM.IntMap (Name a)
