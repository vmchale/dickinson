{-# LANGUAGE OverloadedStrings #-}

-- | This module is loosely based off /Warnings for pattern matching/ by Luc
-- Maranget
module Language.Dickinson.Pattern.Useless ( PatternM
                                          , PatternEnv
                                          , runPatternM
                                          , isExhaustive
                                          , patternEnvDecls
                                          , useful
                                          -- * Exported for testing
                                          , specializeTuple
                                          , specializeTag
                                          ) where

import           Control.Monad              (forM_)
import           Control.Monad.State.Strict (State, execState)
import           Data.Coerce                (coerce)
import           Data.Foldable              (toList, traverse_)
import           Data.Functor               (void)
import           Data.IntMap.Strict         (findWithDefault)
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import           Language.Dickinson.Name
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying)

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

-- TODO: just reader monad... writer at beginning?
type PatternM = State PatternEnv

runPatternM :: PatternM a -> PatternEnv
runPatternM = flip execState (PatternEnv mempty mempty)

-- given a constructor name, get the IntSet of all constructors of that type
assocUniques :: PatternEnv -> Name a -> IS.IntSet
assocUniques env (Name _ (Unique i) _) = {-# SCC "assocUniques" #-}
    let ty = findWithDefault internalError i (types env)
        in findWithDefault internalError ty (allCons env)

internalError :: a
internalError = error "Internal error: lookup in a PatternEnv failed"

isExhaustive :: PatternEnv -> [Pattern a] -> Bool
isExhaustive env ps = {-# SCC "isExhaustive" #-} not $ useful env ps (Wildcard undefined)

isCompleteSet :: PatternEnv -> [Name a] -> Maybe [Name ()]
isCompleteSet _ []       = Nothing
isCompleteSet env ns@(n:_) =
    let allU = assocUniques env n
        ty = coerce (unique <$> ns)
        in if IS.null (allU IS.\\ IS.fromList ty)
            then Just ((\u -> Name undefined (Unique u) ()) <$> IS.toList allU)
            else Nothing

useful :: PatternEnv -> [Pattern a] -> Pattern a -> Bool
useful env ps p = usefulMaranget env [[p'] | p' <- ps] [p]

sanityFailed :: a
sanityFailed = error "Sanity check failed! Perhaps you ran the pattern match exhaustiveness checker on an ill-typed program?"

specializeTag :: Name a -> [[Pattern a]] -> [[Pattern a]]
specializeTag c = concatMap withRow
    where withRow (PatternCons _ c':ps) | c' == c = [ps]
                                        | otherwise = []
          withRow (PatternTuple{}:_)    = sanityFailed
          withRow (Wildcard{}:ps)       = [ps]
          withRow (PatternVar{}:ps)     = [ps]
          withRow (OrPattern _ rs:ps)   = specializeTag c [r:ps | r <- toList rs] -- TODO: unit test case for this
          withRow []                    = emptySpecialize

specializeTuple :: Int -> [[Pattern a]] -> [[Pattern a]]
specializeTuple n = concatMap withRow
    where withRow (PatternTuple _ ps:ps') = [toList ps ++ ps']
          withRow (p@Wildcard{}:ps')      = [replicate n p ++ ps']
          withRow (p@PatternVar{}:ps')    = [replicate n p ++ ps']
          withRow (OrPattern _ rs:ps)     = specializeTuple n [r:ps | r <- toList rs]
          withRow (PatternCons{}:_)       = sanityFailed
          withRow []                      = emptySpecialize

emptySpecialize :: a
emptySpecialize = error "Internal error: tried to take specialized matrix of an empty row"

-- | \\( \matcal(D) \\) in the Maranget paper
defaultMatrix :: [[Pattern a]] -> [[Pattern a]]
defaultMatrix = concatMap withRow where
    withRow []                  = error "Internal error: tried to take default matrix of an empty row"
    withRow (PatternTuple{}:_)  = error "Sanity check failed!" -- because a tuple would be complete by itself
    withRow (PatternCons{}:_)   = []
    withRow (Wildcard{}:ps)     = [ps]
    withRow (PatternVar{}:ps)   = [ps]
    withRow (OrPattern _ rs:ps) = defaultMatrix [r:ps | r <- toList rs]

data Complete a = NotComplete
                | CompleteTuple Int
                | CompleteTags [Name a]

extrCons :: Pattern a -> [Name a]
extrCons (PatternCons _ c) = [c]
extrCons (OrPattern _ ps)  = concatMap extrCons (toList ps)
extrCons _                 = []

-- Is the first column of the pattern matrix complete?
fstComplete :: PatternEnv -> [[Pattern a]] -> Complete ()
fstComplete env ps = {-# SCC "fstComplete" #-}
    if maxTupleLength > 0
        then CompleteTuple maxTupleLength
        else maybe NotComplete CompleteTags
                $ isCompleteSet env (concatMap extrCons fstColumn)
    where fstColumn = fmap head ps
          tuple (PatternTuple _ ps') = length ps'
          tuple (OrPattern _ ps')    = maximum (tuple <$> ps')
          tuple _                    = 0
          maxTupleLength = maximum (tuple <$> fstColumn)

-- follows maranget paper
usefulMaranget :: PatternEnv -> [[Pattern a]] -> [Pattern a] -> Bool
usefulMaranget _ [] _                       = True
usefulMaranget _ _ []                       = False
usefulMaranget env ps (PatternCons _ c:qs)    = usefulMaranget env (specializeTag c ps) qs
usefulMaranget env ps (PatternTuple _ ps':qs) = usefulMaranget env (specializeTuple (length ps') ps) (toList ps' ++ qs)
usefulMaranget env ps (OrPattern _ ps':qs)    = any (\p -> usefulMaranget env ps (p:qs)) ps'
usefulMaranget env ps (q:qs)                  = -- var or wildcard
    let cont = fstComplete env ps in
    case cont of
        NotComplete     -> usefulMaranget env (defaultMatrix ps) qs
        CompleteTuple n -> usefulMaranget env (specializeTuple n ps) (specializeTupleVector n q qs)
        CompleteTags ns -> or $ fmap (\n -> usefulMaranget env (specializeTag n (forget ps)) (fmap void qs)) ns

specializeTupleVector :: Int -> Pattern a -> [Pattern a] -> [Pattern a]
specializeTupleVector n p ps = {-# SCC "specializeTupleVector" #-} replicate n p ++ ps

forget :: [[Pattern a]] -> [[Pattern ()]]
forget = fmap (fmap void)
