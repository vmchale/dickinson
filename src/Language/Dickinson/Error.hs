module Language.Dickinson.Error ( DickinsonError (..)
                                ) where

-- type error but I'll do that later
data DickinsonError name a = UnfoundName a (name a)
                           | TypeMismatch a (Expression name a) DickinsonTy DickinsonTy
