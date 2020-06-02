{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               , DickinsonTy (..)
                               ) where

import           Data.Foldable             (toList)
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Doc, Pretty (pretty), brackets,
                                            dquotes, hsep, langle, parens, pipe,
                                            sep, (<+>))

type Dickinson name a = [Declaration name a]

data Declaration name a = Define a (name a) (Expression name a)

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
    pretty (Define _ n e) = parens (":def" <+> pretty n <+> pretty e)

prettyLetLeaf :: Pretty (name a) => (name a, Expression name a) -> Doc b
prettyLetLeaf (n, e) = brackets (pretty n <+> pretty e)

prettyChoiceBranch :: Pretty (name a) => (Double, Expression name a) -> Doc b
prettyChoiceBranch (d, e) = parens (pipe <+> pretty d <+> pretty e)

instance Pretty (name a) => Pretty (Expression name a) where
    pretty (Var _ n)     = pretty n
    pretty (Literal _ l) = dquotes $ pretty l
    pretty (Let _ ls e) = parens (":let" <+> (sep (toList $ fmap prettyLetLeaf ls)) <+> pretty e)
    pretty (Choice _ brs) = parens (":branch" <+> (sep (toList $ fmap prettyChoiceBranch brs)))
    pretty (Concat _ es) = parens (pipe <> langle <+> hsep (toList $ fmap pretty es))
