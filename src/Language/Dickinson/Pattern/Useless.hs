module Language.Dickinson.Pattern.Useless ( useful
                                          ) where

import           Control.Monad             (forM_, zipWithM)
import           Control.Monad.State       (State, get)
import           Data.Foldable             (toList)
import           Data.IntMap               (findWithDefault)
import qualified Data.IntMap               as IM
import qualified Data.IntSet               as IS
import           Data.List                 (transpose)
import           Data.List.Ext
import           Data.Maybe                (mapMaybe)
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                (Lens')
import           Lens.Micro.Mtl            (modifying)

-- all constructors of a
data PatternEnv = PatternEnv { allCons :: IM.IntMap IS.IntSet -- ^ all constructors indexed by type
                             , types   :: IM.IntMap Int -- ^ all types indexed by constructor
                             }

allConsLens :: Lens' PatternEnv (IM.IntMap IS.IntSet)
allConsLens f s = fmap (\x -> s { allCons = x }) (f (allCons s))

typesLens :: Lens' PatternEnv (IM.IntMap Int)
typesLens f s = fmap (\x -> s { types = x }) (f (types s))

declAdd :: Declaration a -> PatternM ()
declAdd Define{}         = pure ()
declAdd (TyDecl l tn@(Name _ (Unique i) _) cs) =
    forM_ cs $ \(Name _ (Unique j) _) ->
        modifying typesLens (IM.insert j i)

type PatternM = State PatternEnv

isWildcard :: Pattern a -> Bool
isWildcard Wildcard{}   = True
isWildcard PatternVar{} = True
isWildcard _            = False

extrCons :: [Pattern a] -> [TyName a]
extrCons = concat . mapMaybe g where
    g (PatternCons _ c) = Just [c]
    g (OrPattern _ ps)  = Just $ extrCons (toList ps)
    g _                 = Nothing

-- given a constructor name, get the IntSet of all constructors of that type
assocUniques :: Name a -> PatternM IS.IntSet
assocUniques (Name _ (Unique i) _) = do
    st <- get
    let ty = findWithDefault (error "Internal error in pattern-match coverage checker.") i (types st)
    pure $ findWithDefault (error "Internal error in pattern-match coverage checker.") ty (allCons st)

-- make sure the input is not a wilcard/var!
columinzeExt :: Pattern a -> [Pattern a]
columinzeExt (PatternTuple _ ps) = toList ps
columinzeExt OrPattern{}         = error "TODO not yet implemented."
columinzeExt _                   = error "Pattern-match coverage checker does not work on ill-typed programs."

columinzeBlock :: [Pattern a] -> [[Pattern a]]
columinzeBlock = transpose . fmap columinzeExt

isExhaustive :: [Pattern a] -> PatternM Bool
isExhaustive ps                       | any isWildcard ps = pure True
isExhaustive ps@((PatternCons _ c):_) = do
    let allUniques = unUnique . unique <$> extrCons ps
    pAll <- assocUniques c
    pure $ IS.null $ pAll IS.\\ (IS.fromList allUniques)
isExhaustive ps@(PatternTuple{}:_)    = allA isExhaustive (columinzeBlock ps)
isExhaustive ((OrPattern _ ps):ps')   = isExhaustive (toList ps ++ ps')

useful :: [Pattern a] -> Pattern a -> PatternM Bool
useful [] _                    = pure True
useful ps (OrPattern _ ps')    = anyA (useful ps) ps' -- all?
useful ps PatternTuple{}       | any isWildcard ps = pure False -- so we can call columinzeBlock without fear
useful ps (PatternTuple _ ps') = and <$> zipWithM useful (columinzeBlock ps) (toList ps')
useful ps Wildcard{}           = not <$> isExhaustive ps
useful ps PatternVar{}         = not <$> isExhaustive ps
useful ps (PatternCons _ c)    = pure $
    not (any isWildcard ps)
        && c `notElem` extrCons ps -- TODO: do this in one traversal
