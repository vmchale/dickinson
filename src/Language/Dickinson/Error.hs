{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Error ( DickinsonError (..)
                                ) where

import           Control.DeepSeq           (NFData)
import           Control.Exception         (Exception)
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Pretty (pretty), squotes, (<+>))
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Type

-- type error but I'll do that later
data DickinsonError a = UnfoundName a (Name a)
                      | NoText T.Text -- separate from UnfoundName since there is no loc
                      | MultipleNames a (Name a) -- TODO: throw both?
                      | ParseErr (ParseError a)
                      | ModuleNotFound a (Name a)
                      | TypeMismatch (Expression Name a) (DickinsonTy a) (DickinsonTy a) -- TODO: location information
                      deriving (Generic, NFData)

instance (Pretty a) => Show (DickinsonError a) where
    show = show . pretty

instance (Pretty a) => Pretty (DickinsonError a) where
    pretty (UnfoundName l n)       = pretty n <+> "at" <+> pretty l <+> "is not in scope."
    pretty (NoText t)              = squotes (pretty t) <+> "not defined"
    pretty (ParseErr e)            = pretty e
    pretty (MultipleNames l n)     = pretty n <+> "at" <+> pretty l <+> "has already been defined"
    pretty (TypeMismatch e ty ty') = "Expected" <+> pretty e <+> "to have type" <+> pretty ty <> ", found type" <+> pretty ty'
    pretty (ModuleNotFound l n)    = "Module" <+> pretty n <> ", referenced at" <+> pretty l <> ", not found"

instance (Pretty a, Typeable a) => Exception (DickinsonError a)
