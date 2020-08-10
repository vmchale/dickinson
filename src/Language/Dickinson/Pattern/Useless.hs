-- see
module Language.Dickinson.Pattern.Useless ( useful
                                          ) where

import           Control.Monad.State     (State)
import           Data.Foldable           (toList)
import qualified Data.IntMap             as IM
import qualified Data.IntSet             as IS
import           Data.Maybe              (mapMaybe)
import           Language.Dickinson.Name
import           Language.Dickinson.Type

-- all constructors of a
data PatternEnv a = PatternEnv { allCons :: IM.IntMap IS.IntSet -- ^ all constructors indexed by type
                               , types   :: IM.IntMap Int -- ^ all types indexed by constructor
                               }

type PatternM a = State (PatternEnv a)

extrCons :: [Pattern a] -> [TyName a]
extrCons = concat . mapMaybe g where
    g (PatternVar _ c) = Just [c]
    g (OrPattern _ ps) = Just $ extrCons (toList ps)
    g _                = Nothing

useful :: [Pattern a] -> Pattern a -> Bool
useful [] _                 = True
useful ps (OrPattern _ ps') = any (useful ps) ps' -- all?
useful ps (PatternCons _ c) = c `notElem` extrCons ps
