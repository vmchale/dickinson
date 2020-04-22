module Language.Dickinson.Type ( Dickinson
                               , Declaration (..)
                               , Expression (..)
                               , Type (..)
                               , BuiltinType (..)
                               ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text          as T

-- lol functorial modules
-- also fuck I need English (?) grammatical features -> at the very least,
-- "agreement" -> focus on English tbh
--
-- probability-wise, might be good to look at Hakaru, monadic probability
-- especially Hakaru (basically monadic text for types idk)
--
-- interpolation might be nice
--
-- -> might want a real type system

type Dickinson tyname name a = [Declaration tyname name a]

data Declaration tyname name a = Define a (name a) (Expression tyname name a)
                               | TyDefine (tyname a) (Type tyname name a)

data BuiltinType = TyText
                 | TyProbability
                 deriving Eq

data Type name tyname a = SumType a [tyname a]
                        | RecordType a [(name a, tyname a)]
                        | TyVar a (tyname a)
                        | TyBuildtin a !BuiltinType

data Expression tyname name a = Literal a !T.Text
                              | Choice a !(NonEmpty (Double, Expression tyname name a))
                              | Let a ![(name a, Maybe (Type tyname name a), Expression tyname name a)] !(Expression tyname name a)
                              | Var a (name a)
