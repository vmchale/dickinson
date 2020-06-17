{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               , DickinsonTy (..)
                               ) where

import           Control.DeepSeq               (NFData)
import           Data.Binary                   (Binary)
import           Data.Foldable                 (toList)
import           Data.List.NonEmpty            (NonEmpty)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Doc, Pretty (pretty), brackets, dquotes, group, indent, parens, pipe,
                                                vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Ext (hardSep, (<#*>), (<#>), (<^>))
import           GHC.Generics                  (Generic)

type Dickinson name a = [Declaration name a]

data Declaration name a = Define { declAnn :: a
                                 , defName :: name a
                                 , defExpr :: Expression name a
                                 }
                        | Import { declAnn :: a
                                 , declMod :: !(name a)
                                 }
                        deriving (Generic, NFData, Binary, Functor)

data Expression name a = Literal a !T.Text
                       | StrChunk a !T.Text
                       | Choice a !(NonEmpty (Double, Expression name a))
                       | Let a !(NonEmpty (name a, Expression name a)) !(Expression name a)
                       | Var a (name a)
                       | Interp a ![Expression name a]
                       | Lambda a (name a) (DickinsonTy a) (Expression name a) -- TODO: application, type checker
                       | Apply (Expression name a) (Expression name a)
                       deriving (Generic, NFData, Binary, Functor)
                       -- TODO: tuples &. such
                       -- concat back again?
                       -- TODO: normalize subtree
                       -- TODO: builtins?

data DickinsonTy a = TyText a
                   | TyFun a (DickinsonTy a) (DickinsonTy a)
                   deriving (Eq, Generic, NFData, Binary, Functor)

instance Pretty (name a) => Pretty (Declaration name a) where
    pretty (Define _ n e) = parens (":def" <+> pretty n <#> indent 4 (pretty e))
    pretty (Import _ n)   = parens (":import" <+> pretty n)

prettyLetLeaf :: Pretty (name a) => (name a, Expression name a) -> Doc b
prettyLetLeaf (n, e) = group (brackets (pretty n <+> pretty e))

prettyChoiceBranch :: Pretty (name a) => (Double, Expression name a) -> Doc b
prettyChoiceBranch (d, e) = parens (pipe <+> pretty d <+> pretty e)


prettyInterp :: Pretty (name a) => Expression name a -> Doc b
prettyInterp (StrChunk _ t) = pretty t
prettyInterp e              = "${" <> pretty e <> "}"

-- figure out indentation
instance Pretty (name a) => Pretty (Expression name a) where
    pretty (Var _ n)         = pretty n
    pretty (Literal _ l)     = dquotes $ pretty l
    pretty (Let _ ls e)      = group (parens (":let" <^> vsep (toList (fmap prettyLetLeaf ls) ++ [pretty e])))
    -- TODO: if they're all equal, use :oneof
    -- also comments lol
    pretty (Choice _ brs)    = parens (":branch" <#> indent 4 (hardSep (toList $ fmap prettyChoiceBranch brs)))
    pretty (Lambda _ n ty e) = parens (":lambda" <+> pretty n <+> parens (pretty ty) <#*> pretty e)
    pretty (Apply e e')      = pretty e <+> pretty e'
    pretty (Interp _ es)     = dquotes (foldMap prettyInterp es)
    pretty StrChunk{}        = error "Internal error: naked StrChunk"

instance Pretty (DickinsonTy a) where
    pretty TyText{}       = "text"
    pretty (TyFun _ t t') = "⟶" <+> pretty t <+> pretty t'
