{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               , DickinsonTy (..)
                               ) where

import           Data.Foldable                 (toList)
import           Data.List.NonEmpty            (NonEmpty)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Doc, Pretty (pretty), brackets,
                                                dquotes, group, hsep, indent,
                                                langle, parens, pipe, sep, vsep,
                                                (<+>))
import           Data.Text.Prettyprint.Doc.Ext ((<#>), (<:>), (<^>))

type Dickinson name a = [Declaration name a]

data Declaration name a = Define { defAnn  :: a
                                 , defName :: (name a)
                                 , defExpr :: (Expression name a)
                                 }

data Expression name a = Literal a !T.Text
                       | Choice a !(NonEmpty (Double, Expression name a))
                       | Let a !(NonEmpty (name a, Expression name a)) !(Expression name a)
                       | Var a (name a)
                       | Concat a !(NonEmpty (Expression name a))
                       -- TODO: normalize subtree

data DickinsonTy = Text
                 | Fun DickinsonTy DickinsonTy
                 -- lol don't even have functions

instance Pretty (name a) => Pretty (Declaration name a) where
    pretty (Define _ n e) = parens (":def" <+> pretty n <#> indent 4 (pretty e))

prettyLetLeaf :: Pretty (name a) => (name a, Expression name a) -> Doc b
prettyLetLeaf (n, e) = group (brackets (pretty n <+> pretty e))

prettyChoiceBranch :: Pretty (name a) => (Double, Expression name a) -> Doc b
prettyChoiceBranch (d, e) = parens (pipe <+> pretty d <+> pretty e)

-- figure out indentation
instance Pretty (name a) => Pretty (Expression name a) where
    pretty (Var _ n)     = pretty n
    pretty (Literal _ l) = dquotes $ pretty l
    pretty (Let _ ls e) = parens (":let" <^> (vsep (toList (fmap prettyLetLeaf ls) ++ [pretty e])))
    -- TODO: if they're all equal, use :oneof
    -- also comments lol
    pretty (Choice _ brs) = parens (":branch" <#> indent 4 (vsep (toList $ fmap prettyChoiceBranch brs)))
    pretty (Concat _ es) = parens (pipe <> langle <+> hsep (toList $ fmap pretty es))
