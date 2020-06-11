{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.Dickinson.Name ( Name (..)
                               , Unique (..)
                               , NameEnv
                               , isMain
                               ) where

import           Control.DeepSeq               (NFData (..))
import           Data.Foldable                 (toList)
import qualified Data.IntMap                   as IM
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Pretty (pretty))
import           Data.Text.Prettyprint.Doc.Ext (intercalate)

newtype Unique = Unique { unUnique :: Int }
    deriving (Eq, Ord, Pretty, NFData)

data Name a = Name { name   :: NonEmpty T.Text
                   , unique :: !Unique
                   , loc    :: a
                   } deriving (Functor)

instance NFData a => NFData (Name a) where
    rnf (Name _ u x) = rnf x `seq` u `seq` ()

isMain :: Name a -> Bool
isMain = (== ("main" :| [])) . name

instance Eq (Name a) where
    (==) ~(Name _ u _) ~(Name _ u' _) = u == u'

instance Ord (Name a) where
    compare ~(Name _ u _) ~(Name _ u' _) = compare u u'

instance Pretty (Name a) where
    pretty (Name t u _) = intercalate "." (toList (pretty <$> t)) <> "_" <> pretty u

type NameEnv a = IM.IntMap (Name a)
