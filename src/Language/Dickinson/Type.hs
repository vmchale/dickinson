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
import qualified Data.List.NonEmpty            as NE
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

data Expression a = Literal { exprAnn :: a, litText :: T.Text }
                  | StrChunk { exprAnn :: a, chunkText :: T.Text }
                  | Choice { exprAnn :: a
                           , choices :: (NonEmpty (Double, Expression a))
                           }
                  | Let { exprAnn  :: a
                        , letBinds :: (NonEmpty (Name a, Expression a))
                        , letExpr  :: (Expression a)
                        }
                  | Var { exprAnn :: a, exprVar :: (Name a) }
                  | Interp { exprAnn :: a, exprInterp :: [Expression a] }
                  | Lambda { exprAnn    :: a
                           , lambdaVar  :: (Name a)
                           , lambdaTy   :: (DickinsonTy a)
                           , lambdaExpr :: (Expression a)
                           }
                  | Apply { exprAnn :: a
                          , exprFun :: (Expression a)
                          , exprArg :: (Expression a)
                          }
                  | Concat { exprAnn :: a, exprConcats :: [Expression a] }
                  | Tuple { exprAnn :: a, exprTup :: (NonEmpty (Expression a)) }
                  | Match { exprAnn   :: a
                          , exprMatch :: (Expression a)
                          , exprPat   :: (Pattern a)
                          , exprIn    :: (Expression a)
                          }
                  | Flatten { exprAnn :: a, exprFlat :: (Expression a) }
                  | Annot { exprAnn :: a
                          , expr    :: (Expression a)
                          , exprTy  :: (DickinsonTy a)
                          }
                  deriving (Generic, NFData, Binary, Functor, Show)
                  -- TODO: builtins?

data DickinsonTy a = TyText a
                   | TyFun a (DickinsonTy a) (DickinsonTy a)
                   | TyTuple a (NonEmpty (DickinsonTy a))
                   | TyNamed a (Name a)
                   deriving (Generic, NFData, Binary, Show, Functor)

instance Eq (DickinsonTy a) where
    (==) TyText{} TyText{}                     = True
    (==) (TyFun _ ty ty') (TyFun _ ty'' ty''') = (ty == ty'') && (ty' == ty''')
    (==) (TyTuple _ tys) (TyTuple _ tys')      = and (NE.zipWith (==) tys tys')
    (==) (TyNamed _ n) (TyNamed _ n')          = n == n'
    (==) _ _                                   = False

instance Pretty (Declaration a) where
    pretty (Define _ n e) = parens (":def" <+> pretty n <#> indent 2 (pretty e))

instance Pretty (Import a) where
    pretty (Import _ n)   = parens (":include" <+> pretty n)

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

instance Pretty (DickinsonTy a) where
    pretty TyText{}       = "text"
    pretty (TyFun _ t t') = parens ("⟶" <+> pretty t <+> pretty t')
    pretty (TyTuple _ ts) = tupled (toList (pretty <$> ts))
    pretty (TyNamed _ n)  = pretty n
