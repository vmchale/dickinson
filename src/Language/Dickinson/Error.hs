{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Error ( DickinsonError (..)
                                , DickinsonWarning (..)
                                , maybeThrow
                                ) where

import           Control.DeepSeq           (NFData)
import           Control.Exception         (Exception)
import           Control.Monad.Except      (MonadError, throwError)
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Type
import           Prettyprinter             (Pretty (pretty), dquotes, squotes, (<+>))

data DickinsonError a = UnfoundName a (Name a)
                      | NoText T.Text -- separate from UnfoundName since there is no loc
                      | ParseErr FilePath (ParseError a)
                      | ModuleNotFound a (Name a)
                      | TypeMismatch (Expression a) (DickinsonTy a) (DickinsonTy a)
                      | PatternTypeMismatch (Pattern a) (DickinsonTy a) (DickinsonTy a)
                      | ExpectedLambda (Expression a) (DickinsonTy a)
                      | MultiBind a (Name a) (Pattern a) -- When a variable is bound more than once in a pattern
                      | MalformedTuple a
                      | UnfoundConstructor a (TyName a)
                      | UnfoundType a (Name a)
                      | PatternFail a (Expression a)
                      | SuspectPattern a (Pattern a)
                      deriving (Generic, NFData)

data DickinsonWarning a = MultipleNames a (Name a) -- TODO: throw both?
                        | DuplicateStr a T.Text
                        | InexhaustiveMatch a
                        | UselessPattern a (Pattern a)
                        deriving (Generic, NFData)

maybeThrow :: MonadError e m => Maybe e -> m ()
maybeThrow (Just err) = throwError err
maybeThrow Nothing    = pure ()

instance (Pretty a) => Show (DickinsonError a) where
    show = show . pretty

instance (Pretty a) => Pretty (DickinsonError a) where
    pretty (UnfoundName l n)         = pretty l <+> pretty n <+> "is not in scope."
    pretty (NoText t)                = squotes (pretty t) <+> "not defined"
    pretty (ParseErr _ e)            = pretty e
    pretty (TypeMismatch e ty ty')   = pretty (exprAnn e) <+> "Expected" <+> pretty e <+> "to have type" <+> squotes (pretty ty) <> ", found type" <+> squotes (pretty ty')
    pretty (PatternTypeMismatch p ty ty') = pretty (patAnn p) <+> "Constructor" <+> squotes (pretty p) <+> "has type" <+> squotes (pretty ty') <+> "but must be of type" <+> squotes (pretty ty)
    pretty (ModuleNotFound l n)      = pretty l <+> "Module" <+> squotes (pretty n) <+> "not found"
    pretty (ExpectedLambda e ty)     = pretty (exprAnn e) <+> "Expected" <+> squotes (pretty e) <+> "to be of function type, found type" <+> pretty ty
    pretty (MultiBind l n p)         = pretty l <+> "Name" <+> squotes (pretty n) <+> "is bound more than once in" <+> pretty p
    pretty (MalformedTuple l)        = pretty l <+> "Malformed tuple"
    pretty (UnfoundConstructor l tn) = pretty l <+> "Constructor" <+> squotes (pretty tn) <+> "not found"
    pretty (UnfoundType l ty)        = pretty l <+> "Type" <+> squotes (pretty ty) <+> "not found"
    pretty (PatternFail l e)         = pretty l <+> "Expression" <+> pretty e <+> "failed to match"
    pretty (SuspectPattern l p)      = pretty l <+> "Pattern" <+> squotes (pretty p) <+> "is an or-pattern but it contains a variable."

instance (Pretty a, Typeable a) => Exception (DickinsonError a)

instance (Pretty a) => Show (DickinsonWarning a) where
    show = show . pretty

instance (Pretty a) => Pretty (DickinsonWarning a) where
    pretty (MultipleNames l n)   = pretty n <+> "at" <+> pretty l <+> "has already been defined"
    pretty (DuplicateStr l t)    = pretty l <+> "duplicate string" <+> dquotes (pretty t)
    pretty (InexhaustiveMatch l) = pretty l <+> "Inexhaustive match in expression"
    pretty (UselessPattern l p)  = pretty l <+> "Pattern" <+> pretty p <+> "is redundant"

instance (Pretty a, Typeable a) => Exception (DickinsonWarning a)
