{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.Dickinson.Name ( TyName
                               , Name (..)
                               , NameEnv
                               ) where

import           Control.DeepSeq               (NFData (..))
import           Data.Binary                   (Binary (..))
import           Data.Data                     (Data)
import           Data.Foldable                 (toList)
import qualified Data.IntMap                   as IM
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc.Ext (Debug (..), intercalate)
import           GHC.Generics                  (Generic)
import           Language.Dickinson.Unique
import           Prettyprinter                 (Pretty (pretty))

type TyName a = Name a

-- TODO: separate type for module name
-- | A (possibly qualified) name.
data Name a = Name { name   :: NonEmpty T.Text
                   , unique :: !Unique
                   , loc    :: a
                   } deriving (Functor, Generic, Binary, Show, Data)

instance NFData a => NFData (Name a) where
    rnf (Name _ u x) = rnf x `seq` u `seq` ()

instance Eq (Name a) where
    (==) (Name _ u _) (Name _ u' _) = u == u'

instance Ord (Name a) where
    compare (Name _ u _) (Name _ u' _) = compare u u'

instance Pretty (Name a) where
    pretty (Name t _ _) = intercalate "." (toList (pretty <$> t))

instance Debug (Name a) where
    debug (Name t u _) = intercalate "." (toList (pretty <$> t)) <> pretty u

type NameEnv a = IM.IntMap (Name a)
