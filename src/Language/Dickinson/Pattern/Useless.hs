{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Pattern.Useless ( PatternM
                                          , runPatternM
                                          , isExhaustive
                                          , patternEnvDecls
                                          -- * Exported for testing
                                          , specializeTuple
                                          , specializeTag
                                          ) where

import           Control.Monad             (forM, forM_)
import           Control.Monad.State       (State, evalState, get)
import           Data.Foldable             (toList, traverse_)
import           Data.Functor              (void)
import           Data.IntMap               (findWithDefault)
import qualified Data.IntMap               as IM
import qualified Data.IntSet               as IS
import           Data.List.Ext
import           Data.List.NonEmpty        (NonEmpty ((:|)))
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

-- given a constructor name, get the IntSet of all constructors of that type
assocUniques :: Name a -> PatternM IS.IntSet
assocUniques (Name _ (Unique i) _) = do
    st <- get
    let ty = findWithDefault undefined i (types st)
    pure $ findWithDefault undefined ty (allCons st)

isExhaustive :: [Pattern a] -> PatternM Bool
isExhaustive ps = not <$> useful ps (Wildcard undefined)

isCompleteSet :: [Name a] -> PatternM (Maybe [Name ()])
isCompleteSet []       = pure Nothing
isCompleteSet ns@(n:_) = do
    allU <- assocUniques n
    let ty = unUnique . unique <$> ns
    pure $
        if IS.null (allU IS.\\ IS.fromList ty)
            then Just ((\u -> (Name ("(internal)" :| []) (Unique u) ())) <$> IS.toList allU)
            else Nothing

useful :: [Pattern a] -> Pattern a -> PatternM Bool
useful ps p = usefulMaranget [[p'] | p' <- ps] [p]

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
fstComplete :: [[Pattern a]] -> PatternM (Complete ())
fstComplete ps =
    if maxTupleLength > 0
        then pure $ CompleteTuple maxTupleLength
        else do
            res <- isCompleteSet (concatMap extrCons fstColumn)
            pure $ case res of
                Just ns -> CompleteTags ns
                Nothing -> NotComplete
    where fstColumn = fmap head ps
          tuple (PatternTuple _ ps') = length ps'
          tuple (OrPattern _ ps')    = maximum (tuple <$> ps')
          tuple _                    = 0
          maxTupleLength = maximum (tuple <$> fstColumn)

-- follows maranget paper
usefulMaranget :: [[Pattern a]] -> [Pattern a] -> PatternM Bool
usefulMaranget [] _                       = pure True
usefulMaranget _ []                       = pure False
usefulMaranget ps (PatternCons _ c:qs)    = usefulMaranget (specializeTag c ps) qs
usefulMaranget ps (PatternTuple _ ps':qs) = usefulMaranget (specializeTuple (length ps') ps) (toList ps' ++ qs)
usefulMaranget ps (OrPattern _ ps':qs)    = forAnyA ps' $ \p -> usefulMaranget ps (p:qs)
usefulMaranget ps (q@Wildcard{}:qs)       = do
    cont <- fstComplete ps
    case cont of
        NotComplete     -> usefulMaranget (defaultMatrix ps) qs
        CompleteTuple n -> usefulMaranget (specializeTuple n ps) (specializeTupleVector n q qs)
        CompleteTags ns -> or <$> (forM ns $ \n -> usefulMaranget (specializeTag n (forget ps)) (fmap void qs))
usefulMaranget ps (q@PatternVar{}:qs)     = do
    cont <- fstComplete ps
    case cont of
        NotComplete     -> usefulMaranget (defaultMatrix ps) qs
        CompleteTuple n -> usefulMaranget (specializeTuple n ps) (specializeTupleVector n q qs)
        CompleteTags ns -> or <$> (forM ns $ \n -> usefulMaranget (specializeTag n (forget ps)) (fmap void qs))

specializeTupleVector :: Int -> Pattern a -> [Pattern a] -> [Pattern a]
specializeTupleVector n p ps = replicate n p ++ ps

forget :: [[Pattern a]] -> [[Pattern ()]]
forget = fmap (fmap void)
