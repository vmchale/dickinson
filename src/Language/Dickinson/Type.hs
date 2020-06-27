{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Type ( Dickinson (..)
                               , Declaration (..)
                               , Import (..)
                               , Expression (..)
                               , Pattern (..)
                               , DickinsonTy (..)
                               ) where

import           Control.DeepSeq               (NFData)
import           Data.Binary                   (Binary)
import           Data.Foldable                 (toList)
import           Data.List.NonEmpty            (NonEmpty)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Doc, Pretty (pretty), brackets, colon, concatWith, dquotes, group,
                                                hardline, hsep, indent, parens, pipe, rangle, tupled, vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Ext (hardSep, (<#*>), (<#>), (<^>))
import           GHC.Generics                  (Generic)
import           Language.Dickinson.Name

data Dickinson a = Dickinson { modImports :: [Import a]
                             , modDefs    :: [Declaration a]
                             } deriving (Generic, NFData, Binary, Functor, Show)

data Declaration a = Define { declAnn :: a
                            , defName :: Name a
                            , defExpr :: Expression a
                            }
                            deriving (Generic, NFData, Binary, Functor, Show)

data Import a = Import { importAnn :: a
                       , declMod   :: Name a
                       }
                       deriving (Generic, NFData, Binary, Functor, Show)

data Pattern a = PatternVar a (Name a)
               | PatternTuple a (NonEmpty (Pattern a))
               | Wildcard a
               deriving (Generic, NFData, Binary, Functor, Show)

data Expression a = Literal a T.Text
                  | StrChunk a T.Text
                  | Choice a (NonEmpty (Double, Expression a))
                  | Let a (NonEmpty (Name a, Expression a)) (Expression a)
                  | Var a (Name a)
                  | Interp a [Expression a]
                  | Lambda a (Name a) DickinsonTy (Expression a)
                  | Apply a (Expression a) (Expression a)
                  | Concat a [Expression a]
                  | Tuple a (NonEmpty (Expression a))
                  | Match a (Expression a) (Pattern a) (Expression a)
                  | Flatten a (Expression a)
                  | Annot a (Expression a) DickinsonTy
                  deriving (Generic, NFData, Binary, Functor, Show)
                  -- TODO: builtins?

data DickinsonTy = TyText
                 | TyFun DickinsonTy DickinsonTy
                 | TyTuple (NonEmpty DickinsonTy)
                 deriving (Eq, Generic, NFData, Binary, Show)

instance Pretty (Declaration a) where
    pretty (Define _ n e) = parens (":def" <+> pretty n <#> indent 2 (pretty e))

instance Pretty (Import a) where
    pretty (Import _ n)   = parens (":import" <+> pretty n)

instance Pretty (Dickinson a) where
    pretty (Dickinson is ds) = concatWith (\x y -> x <> hardline <> hardline <> y) (fmap pretty is <> ["%-"] <> fmap pretty ds)

prettyLetLeaf :: (Name a, Expression a) -> Doc b
prettyLetLeaf (n, e) = group (brackets (pretty n <+> pretty e))

prettyChoiceBranch :: (Double, Expression a) -> Doc b
prettyChoiceBranch (d, e) = parens (pipe <+> pretty d <+> pretty e)


prettyInterp :: Expression a -> Doc b
prettyInterp (StrChunk _ t) = pretty t
prettyInterp e              = "${" <> pretty e <> "}"

instance Pretty (Pattern a) where
    pretty (PatternVar _ n)    = pretty n
    pretty (PatternTuple _ ps) = tupled (toList (pretty <$> ps))
    pretty Wildcard{}          = "_"

-- figure out indentation
instance Pretty (Expression a) where
    pretty (Var _ n)         = pretty n
    pretty (Literal _ l)     = dquotes $ pretty l
    pretty (Let _ ls e)      = group (parens (":let" <^> vsep (toList (fmap prettyLetLeaf ls) ++ [pretty e])))
    -- TODO: if they're all equal, use :oneof
    -- also comments lol
    pretty (Choice _ brs)    = parens (":branch" <#> indent 2 (hardSep (toList $ fmap prettyChoiceBranch brs)))
    pretty (Lambda _ n ty e) = parens (":lambda" <+> pretty n <+> pretty ty <#*> pretty e)
    pretty (Apply _ e e')    = parens ("$" <+> pretty e <+> pretty e')
    pretty (Interp _ es)     = group (dquotes (foldMap prettyInterp es))
    pretty (Concat _ es)     = parens (rangle <+> hsep (pretty <$> es))
    pretty StrChunk{}        = error "Internal error: naked StrChunk"
    pretty (Tuple _ es)      = tupled (toList (pretty <$> es))
    pretty (Match _ n p e)   = parens (":match" <+> pretty n <^> pretty p <^> pretty e)
    pretty (Flatten _ e)     = parens (":flatten" <^> pretty e)
    pretty (Annot _ e ty)    = pretty e <+> colon <+> pretty ty

instance Pretty DickinsonTy where
    pretty TyText{}     = "text"
    pretty (TyFun t t') = parens ("⟶" <+> pretty t <+> pretty t')
    pretty (TyTuple ts) = tupled (toList (pretty <$> ts))
