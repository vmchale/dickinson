module Language.Dickinson.Pattern.Useless ( PatternM
                                          , runPatternM
                                          , isExhaustive
                                          , patternEnvDecls
                                          ) where

import           Control.Monad             (forM_)
import           Control.Monad.State       (State, evalState, get)
import           Data.Foldable             (toList, traverse_)
import           Data.IntMap               (findWithDefault)
import qualified Data.IntMap               as IM
import qualified Data.IntSet               as IS
import           Data.List.Ext
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NE
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
declAdd (TyDecl _ (Name _ (Unique i) _) cs) = do
    forM_ cs $ \(Name _ (Unique j) _) ->
        modifying typesLens (IM.insert j i)
    let cons = IS.fromList $ toList (unUnique . unique <$> cs)
    modifying allConsLens (IM.insert i cons)

patternEnvDecls :: [Declaration a] -> PatternM ()
patternEnvDecls = traverse_ declAdd

type PatternM = State PatternEnv

runPatternM :: PatternM a -> a
runPatternM = flip evalState (PatternEnv mempty mempty)

isWildcard :: Pattern a -> Bool
isWildcard Wildcard{}   = True
isWildcard PatternVar{} = True
-- FIXME: or pattern containing wildcard? -> sketchy
isWildcard _            = False

extrCons :: [Pattern a] -> [Name a]
extrCons = concat . mapMaybe g where
    g (PatternCons _ c) = Just [c]
    g (OrPattern _ ps)  = Just $ extrCons (toList ps)
    g _                 = Nothing

internalError :: a
internalError = error "Internal error in pattern-match coverage checker."

tyError :: a
tyError = error "Exhaustiveness checker does not work on ill-typed programs."

errTup :: a
errTup = error "Tuple must have at least two elements."

-- given a constructor name, get the IntSet of all constructors of that type
assocUniques :: Name a -> PatternM IS.IntSet
assocUniques (Name _ (Unique i) _) = do
    st <- get
    let ty = findWithDefault internalError i (types st)
    pure $ findWithDefault internalError ty (allCons st)

isExhaustive :: [Pattern a] -> PatternM Bool
isExhaustive ps = not <$> useful ps (Wildcard undefined)

isCompleteSet :: [Name a] -> PatternM Bool
isCompleteSet []       = pure False
isCompleteSet ns@(n:_) = do
    allU <- assocUniques n
    let ty = unUnique . unique <$> ns
    pure $ IS.null (allU IS.\\ IS.fromList ty)

isComplete :: [Pattern a] -> PatternM Bool
isComplete = isCompleteSet . extrCons

-- do the first columns form a complete set?
fstComplete :: [Pattern a] -> PatternM Bool
fstComplete = isComplete . mapMaybe extrFst
    where extrFst (PatternTuple _ (p :| _)) = Just p
          extrFst _                         = Nothing

-- Specialize a stack of patterns w.r.t. a constructor pattern
specializeConstructor :: Pattern a -- ^ Constructor
                      -> [Pattern a]
                      -> [Pattern a]
specializeConstructor (PatternCons _ c) = concatMap unstitch
    where unstitch (PatternTuple _ (_ :| []))                   = errTup
          unstitch (PatternTuple _ ((PatternCons _ c') :| [p])) | c' == c = [p]
                                                                | otherwise = []
          unstitch (PatternTuple _ (Wildcard{} :| [p]))         = [p]
          unstitch (PatternTuple _ (PatternVar{} :| [p]))       = [p]
          unstitch (PatternTuple l ((PatternCons _ c') :| ps))  | c == c' = [PatternTuple l $ NE.fromList ps]
                                                                | otherwise = []
          unstitch (PatternTuple l (Wildcard{} :| ps))          = [PatternTuple l $ NE.fromList ps]
          unstitch (PatternTuple l (PatternVar{} :| ps))        = [PatternTuple l $ NE.fromList ps]
          unstitch OrPattern{}                                  = undefined
specializeConstructor _                  = internalError

-- "un-stitch" or patterns... specialized matrix
useful :: [Pattern a] -> Pattern a -> PatternM Bool
useful [] _                                           = pure True
useful ps (OrPattern _ ps')                           = anyA (useful ps) ps' -- all?
useful ps _                                           | any isWildcard ps = pure False -- check for wildcards so that stripRelevant only gets tuples
useful ps (PatternCons _ c)                           = pure $ c `notElem` extrCons ps -- already checked for wildcards
useful _ (PatternTuple  _ (_ :| []))                  = errTup -- TODO: loc
useful ps@(PatternCons{}:_) Wildcard{}                = not <$> isComplete ps
useful ps@(PatternCons{}:_) PatternVar{}              = not <$> isComplete ps
useful ps@(PatternTuple{}:_) Wildcard{}               = undefined -- "complete signature" in Maranget paper includes (,) as well as c_k
useful ps@(PatternTuple{}:_) PatternVar{}             = undefined
useful ps@(OrPattern{}:_) Wildcard{}                  = undefined
useful ps@(OrPattern{}:_) PatternVar{}                = undefined
useful ps (PatternTuple _ (Wildcard{} :| ps'))        = undefined
useful ps (PatternTuple _ (PatternVar{} :| ps'))      = undefined
useful ps (PatternTuple _ ((PatternCons _ c) :| [p])) = useful (fmap (stripRelevant c) ps) p
useful ps (PatternTuple l ((PatternCons _ c) :| ps')) = useful (fmap (stripRelevant c) ps) (PatternTuple l $ NE.fromList ps')
useful ps (PatternTuple _ (OrPattern{} :| ps'))       = undefined
useful ps (PatternTuple _ (PatternTuple _ p :| ps'))  = undefined

-- strip a pattern (presumed to be a constructor or or-pattern) to relevant parts
stripRelevant :: Name a -> Pattern a -> Pattern a
stripRelevant _ (PatternTuple _ (_ :| []))                   = errTup -- TODO: loc
stripRelevant c (PatternTuple _ ((PatternCons _ c') :| [p])) | c' == c = p
stripRelevant _ (PatternTuple _ (PatternVar{} :| [p]))       = p
stripRelevant _ (PatternTuple _ (Wildcard{} :| [p]))         = p
stripRelevant c (PatternTuple _ ((OrPattern _ ps) :| [p]))   | c `elem` extrCons (toList ps) = p
stripRelevant c (PatternTuple l ((PatternCons _ c') :| ps))  | c' == c = PatternTuple l (NE.fromList ps)
stripRelevant c (PatternTuple l ((OrPattern _ ps) :| ps'))   | c `elem` extrCons (toList ps) = PatternTuple l (NE.fromList ps')
stripRelevant _ (PatternTuple l (PatternVar{} :| ps))        = PatternTuple l (NE.fromList ps)
stripRelevant _ (PatternTuple l (Wildcard{} :| ps))          = PatternTuple l (NE.fromList ps)
stripRelevant _ OrPattern{}                                  = undefined -- re-stitch it basically?
stripRelevant _ _                                            = tyError -- if we call stripRelevant on a non-tuple, that means a constructor was "above" a tuple, which is
                                                                       -- ill-typed anyway. Also, we've already checked for wildcards/vars in useful ^
