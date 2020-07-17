module Language.Dickinson.Pattern.Useless ( useful
                                          ) where

import           Data.Array              (Array, bounds)
import           Data.Foldable           (toList)
import qualified Data.IntMap             as IM
import qualified Data.IntSet             as IS
import           Data.Maybe              (mapMaybe)
import           Language.Dickinson.Name
import           Language.Dickinson.Type

data PatternEnv = PatternEnv { allCons :: IM.IntMap IS.IntSet -- ^ all constructors indexed by type
                             , types   :: IM.IntMap Int -- ^ type indexed by constructor
                             }

extrCons :: [Pattern a] -> [TyName a]
extrCons = concat . mapMaybe g where
    g (PatternVar _ c) = Just [c]
    g (OrPattern _ ps) = Just $ extrCons (toList ps)
    g _                = Nothing

-- specializied? lol -> array library?

useful' :: [Pattern a] -> Pattern a -> Bool
useful' [] _                 = True
useful' ps (OrPattern _ ps') = any (useful' ps) ps' -- all?
useful' ps (PatternCons _ c) = c `notElem` extrCons ps

useful :: Array (Int, Int) (Pattern a) -> Array Int (Pattern a) -> Bool
useful p _ | let (_,(_,n)) = bounds p in n == 0 = True
useful p _ | let (_,(m,_)) = bounds p in m > 0 = False
