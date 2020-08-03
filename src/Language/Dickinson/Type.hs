{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.Dickinson.Type ( Dickinson (..)
                               , Declaration (..)
                               , Import (..)
                               , Expression (..)
                               , Pattern (..)
                               , DickinsonTy (..)
                               -- * Accesors
                               , defExprM
                               ) where

import           Control.DeepSeq               (NFData)
import           Data.Binary                   (Binary)
import           Data.Data                     (Data)
import           Data.Foldable                 (toList)
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as NE
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc.Ext (Debug (..), hardSep, (<#*>), (<#>), (<^>))
import           GHC.Generics                  (Generic)
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Prettyprinter                 (Doc, Pretty (pretty), align, brackets, colon, concatWith, dquotes,
                                                encloseSep, group, hardline, hsep, indent, lparen, parens, pipe, rangle,
                                                rparen, tupled, vsep, (<+>))
import           Prettyprinter.Internal        (unsafeTextWithoutNewlines)

data Dickinson a = Dickinson { modImports :: [Import a]
                             , modDefs    :: [Declaration a]
                             } deriving (Generic, NFData, Binary, Functor, Show)

data Declaration a = Define { declAnn :: a
                            , defName :: Name a
                            , defExpr :: Expression a
                            }
                   | TyDecl { declAnn :: a
                            , tyName  :: Name a
                            , tyCons  :: NonEmpty (TyName a)
                            }
                   deriving (Generic, NFData, Binary, Functor, Show, Data)

data Import a = Import { importAnn :: a
                       , declMod   :: Name a
                       }
                       deriving (Generic, NFData, Binary, Functor, Show)

data Pattern a = PatternVar { patAnn :: a, patName :: Name a }
               | PatternTuple { patAnn :: a, patTup :: NonEmpty (Pattern a) }
               | PatternCons { patAnn :: a, patCons :: TyName a }
               | Wildcard { patAnn :: a }
               | OrPattern { patAnn :: a, patOr :: NonEmpty (Pattern a) }
               deriving (Generic, NFData, Binary, Functor, Show, Data)

data Expression a = Literal { exprAnn :: a, litText :: T.Text }
                  | StrChunk { exprAnn :: a, chunkText :: T.Text }
                  | Choice { exprAnn :: a
                           , choices :: NonEmpty (Double, Expression a)
                           }
                  | Let { exprAnn  :: a
                        , letBinds :: NonEmpty (Name a, Expression a)
                        , letExpr  :: Expression a
                        }
                  | Var { exprAnn :: a, exprVar :: Name a }
                  | Interp { exprAnn :: a, exprInterp :: [Expression a] }
                  | MultiInterp { exprAnn :: a, exprMultiInterp :: [Expression a] }
                  | Lambda { exprAnn    :: a
                           , lambdaVar  :: Name a
                           , lambdaTy   :: DickinsonTy a
                           , lambdaExpr :: Expression a
                           }
                  | Apply { exprAnn :: a
                          , exprFun :: Expression a
                          , exprArg :: Expression a
                          }
                  | Concat { exprAnn :: a, exprConcats :: [Expression a] }
                  | Tuple { exprAnn :: a, exprTup :: NonEmpty (Expression a) }
                  | Match { exprAnn    :: a
                          , exprMatch  :: Expression a
                          , exprBranch :: NonEmpty (Pattern a, Expression a)
                          }
                  | Flatten { exprAnn :: a, exprFlat :: Expression a }
                  | Annot { exprAnn :: a
                          , expr    :: Expression a
                          , exprTy  :: DickinsonTy a
                          }
                  | Constructor { exprAnn :: a, constructorName :: TyName a }
                  | BuiltinFn { exprAnn :: a, exprBuiltin :: Builtin }
                  deriving (Generic, NFData, Binary, Functor, Show, Data)
                  -- TODO: builtins?

data DickinsonTy a = TyText a
                   | TyFun a (DickinsonTy a) (DickinsonTy a)
                   | TyTuple a (NonEmpty (DickinsonTy a))
                   | TyNamed a (Name a)
                   deriving (Generic, NFData, Binary, Show, Functor, Data)

instance Eq (DickinsonTy a) where
    (==) TyText{} TyText{}                     = True
    (==) (TyFun _ ty ty') (TyFun _ ty'' ty''') = (ty == ty'') && (ty' == ty''')
    (==) (TyTuple _ tys) (TyTuple _ tys')      = and (NE.zipWith (==) tys tys')
    (==) (TyNamed _ n) (TyNamed _ n')          = n == n'
    (==) _ _                                   = False

instance Pretty (Declaration a) where
    pretty (Define _ n e)  = parens (":def" <+> pretty n <#> indent 2 (pretty e))
    pretty (TyDecl _ n cs) = "tydecl" <+> pretty n <+> "=" <+> concatWith (\x y -> x <+> pipe <+> y) (toList (pretty <$> cs))

instance Pretty (Import a) where
    pretty = prettyImport pretty

prettyImport :: (Name a -> Doc b) -> Import a -> Doc b
prettyImport pn (Import _ n) = parens (":include" <+> pn n)

instance Pretty (Dickinson a) where
    pretty (Dickinson is ds) = concatWith (\x y -> x <> hardline <> hardline <> y) (fmap pretty is <> ["%-"] <> fmap pretty ds)

prettyLetLeaf :: Pretty t => (t, Expression a) -> Doc b
prettyLetLeaf (n, e@MultiInterp{}) = group (brackets (pretty n <^> pretty e))
prettyLetLeaf (n, e@Choice{})      = group (brackets (pretty n <^> pretty e))
prettyLetLeaf (n, e)               = group (brackets (pretty n <+> pretty e))

prettyChoiceBranch :: (Double, Expression a) -> Doc b
prettyChoiceBranch (d, e) = parens (pipe <+> pretty d <+> pretty e)

prettyChoiceOneof :: Expression a -> Doc b
prettyChoiceOneof e = parens (pipe <+> pretty e)

prettyInterp :: Expression a -> Doc b
prettyInterp (StrChunk _ t) = pretty (escReplace t)
prettyInterp e              = "${" <> pretty e <> "}"

prettyMultiInterp :: [Expression a] -> Doc b
prettyMultiInterp = mconcat . fmap prettyChunk
    where prettyChunk (StrChunk _ t) = textHard t
          prettyChunk e              = "${" <> pretty e <> "}"

textHard :: T.Text -> Doc a
textHard = concatWith (<#>) . map unsafeTextWithoutNewlines . T.splitOn "\n"

instance Pretty (Pattern a) where
    pretty = prettyPattern pretty

instance Debug (Pattern a) where
    debug = prettyPattern debug

prettyPattern :: (Name a -> Doc b) -> Pattern a -> Doc b
prettyPattern pn (PatternVar _ n)    = pn n
prettyPattern pn (PatternTuple _ ps) = tupled (toList (prettyPattern pn <$> ps))
prettyPattern _ Wildcard{}           = "_"
prettyPattern pn (PatternCons _ c)   = pn c
prettyPattern pn (OrPattern _ ps)    = group (encloseSep lparen rparen pipe (toList $ fmap (prettyPattern pn) ps))

escReplace :: T.Text -> T.Text
escReplace =
      T.replace "\"" "\\\""
    . T.replace "\n" "\\n"
    . T.replace "${" "\\${"

allEq :: Eq a => NonEmpty a -> Bool
allEq (x :| xs) = all (== x) xs

-- figure out indentation
instance Pretty (Expression a) where
    pretty (Var _ n)          = pretty n
    pretty (Literal _ l)      = dquotes $ pretty (escReplace l)
    pretty (Let _ ls e)       = group (parens (":let" <^> vsep (toList (fmap prettyLetLeaf ls) ++ [pretty e])))
    -- also comments lol
    pretty (Choice _ brs)
        | allEq (fst <$> brs) = parens (":oneof" <#> indent 2 (hardSep (toList $ fmap prettyChoiceOneof (snd <$> brs))))
        | otherwise           = parens (":branch" <#> indent 2 (hardSep (toList $ fmap prettyChoiceBranch brs)))
    pretty (Lambda _ n ty e)  = parens (":lambda" <+> pretty n <+> pretty ty <#*> pretty e)
    pretty (Apply _ e e'@Choice{}) = parens ("$" <+> pretty e <^> pretty e')
    pretty (Apply _ e e')     = parens ("$" <+> pretty e <+> pretty e')
    pretty (Interp _ es)      = group (dquotes (foldMap prettyInterp es))
    pretty (MultiInterp _ es) = group (align ("'''" <> prettyMultiInterp es <> "'''"))
    pretty (Concat _ es)      = parens (rangle <+> hsep (pretty <$> es))
    pretty StrChunk{}         = error "Internal error: naked StrChunk"
    pretty (Tuple _ es)       = tupled (toList (pretty <$> es))
    pretty (Match _ n brs)    = parens (":match" <+> pretty n <^> vsep (toList (fmap prettyLetLeaf brs)))
    pretty (Flatten _ e)      = group (parens (":flatten" <^> pretty e))
    pretty (Annot _ e ty)     = pretty e <+> colon <+> pretty ty
    pretty (Constructor _ tn) = pretty tn
    pretty (BuiltinFn _ b)    = pretty b

instance Pretty (DickinsonTy a) where
    pretty = prettyType pretty

prettyType :: (Name a -> Doc b) -> DickinsonTy a -> Doc b
prettyType _ TyText{}        = "text"
prettyType pn (TyFun _ t t') = parens ("⟶" <+> prettyType pn t <+> prettyType pn t')
prettyType pn (TyTuple _ ts) = tupled (toList (prettyType pn <$> ts))
prettyType pn (TyNamed _ n)  = pn n

defExprM :: Declaration a -> Maybe (Expression a)
defExprM (Define _ _ e) = Just e
defExprM TyDecl{}       = Nothing
