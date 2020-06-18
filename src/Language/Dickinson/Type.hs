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
import           Language.Dickinson.Name

type Dickinson a = [Declaration a]

data Declaration a = Define { declAnn :: a
                            , defName :: Name a
                            , defExpr :: Expression a
                            }
                   | Import { declAnn :: a
                            , declMod :: !(Name a)
                            }
                   deriving (Generic, NFData, Binary, Functor)

data Expression a = Literal a !T.Text
                  | StrChunk a !T.Text
                  | Choice a !(NonEmpty (Double, Expression a))
                  | Let a !(NonEmpty (Name a, Expression a)) !(Expression a)
                  | Var a (Name a)
                  | Interp a ![Expression a]
                  | Lambda a (Name a) (DickinsonTy a) (Expression a) -- TODO: application, type checker
                  | Apply (Expression a) (Expression a)
                  deriving (Generic, NFData, Binary, Functor)
                  -- TODO: tuples &. such
                  -- concat back again?
                  -- TODO: normalize subtree
                  -- TODO: builtins?

data DickinsonTy a = TyText a
                   | TyFun a (DickinsonTy a) (DickinsonTy a)
                   deriving (Eq, Generic, NFData, Binary, Functor)

instance Pretty (Declaration a) where
    pretty (Define _ n e) = parens (":def" <+> pretty n <#> indent 4 (pretty e))
    pretty (Import _ n)   = parens (":import" <+> pretty n)

prettyLetLeaf :: (Name a, Expression a) -> Doc b
prettyLetLeaf (n, e) = group (brackets (pretty n <+> pretty e))

prettyChoiceBranch :: (Double, Expression a) -> Doc b
prettyChoiceBranch (d, e) = parens (pipe <+> pretty d <+> pretty e)


prettyInterp :: Expression a -> Doc b
prettyInterp (StrChunk _ t) = pretty t
prettyInterp e              = "${" <> pretty e <> "}"

-- figure out indentation
instance Pretty (Expression a) where
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
