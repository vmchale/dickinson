{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               , DickinsonTy (..)
                               ) where

import           Control.DeepSeq               (NFData)
import           Data.Foldable                 (toList)
import           Data.List.NonEmpty            (NonEmpty)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Doc, Pretty (pretty), brackets,
                                                dquotes, group, hsep, indent,
                                                langle, parens, pipe, vsep,
                                                (<+>))
import           Data.Text.Prettyprint.Doc.Ext ((<#>), (<^>))
import           GHC.Generics                  (Generic)

type Dickinson name a = [Declaration name a]

data Declaration name a = Define { declAnn :: a
                                 , defName :: name a
                                 , defExpr :: Expression name a
                                 }
                        | Import { declAnn :: a
                                 , declMod :: !(name a)
                                 }
                        deriving (Generic, NFData)

data Expression name a = Literal a !T.Text
                       | Choice a !(NonEmpty (Double, Expression name a))
                       | Let a !(NonEmpty (name a, Expression name a)) !(Expression name a)
                       | Var a (name a)
                       | Concat a !(NonEmpty (Expression name a))
                       deriving (Generic, NFData)
                       -- TODO: normalize subtree
                       -- TODO: builtins?

data DickinsonTy = Text
                 | Fun DickinsonTy DickinsonTy
                 -- lol don't even have functions

instance Pretty (name a) => Pretty (Declaration name a) where
    pretty (Define _ n e) = parens (":def" <+> pretty n <#> indent 4 (pretty e))
    pretty (Import _ n)   = parens (":import" <+> pretty n)

prettyLetLeaf :: Pretty (name a) => (name a, Expression name a) -> Doc b
prettyLetLeaf (n, e) = group (brackets (pretty n <+> pretty e))

prettyChoiceBranch :: Pretty (name a) => (Double, Expression name a) -> Doc b
prettyChoiceBranch (d, e) = parens (pipe <+> pretty d <+> pretty e)

-- figure out indentation
instance Pretty (name a) => Pretty (Expression name a) where
    pretty (Var _ n)     = pretty n
    pretty (Literal _ l) = dquotes $ pretty l
    pretty (Let _ ls e) = parens (":let" <^> vsep (toList (fmap prettyLetLeaf ls) ++ [pretty e]))
    -- TODO: if they're all equal, use :oneof
    -- also comments lol
    pretty (Choice _ brs) = parens (":branch" <^> vsep (toList $ fmap prettyChoiceBranch brs))
    pretty (Concat _ es) = parens (pipe <> langle <+> hsep (toList $ fmap pretty es))
