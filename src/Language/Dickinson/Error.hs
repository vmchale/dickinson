{-# LANGUAGE OverloadedStrings #-}
module Language.Dickinson.Error ( DickinsonError (..)
                                ) where

import           Data.Text.Prettyprint.Doc (Pretty (pretty), squotes, (<+>))
import           Language.Dickinson.Type

-- type error but I'll do that later
data DickinsonError name a = UnfoundName a (name a)
                           | TypeMismatch a (Expression name a) DickinsonTy DickinsonTy
                           | NoMain -- separate from UnfoundName since there is no loc
                           | MultipleNames a (name a) -- top-level identifier defined more than once

instance (Pretty (name a), Pretty a) => Show (DickinsonError name a) where
    show = show . pretty

instance (Pretty (name a), Pretty a) => Pretty (DickinsonError name a) where
    pretty (UnfoundName l n) = pretty n <+> "at" <+> pretty l <+> "is not in scope."
    pretty NoMain            = squotes "main" <+> "not defined"
