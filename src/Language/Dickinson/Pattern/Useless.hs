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

-- given a constructor name, get the IntSet of all constructors of that type
assocUniques :: Name a -> PatternM IS.IntSet
assocUniques (Name _ (Unique i) _) = do
    st <- get
    let ty = findWithDefault undefined i (types st)
    pure $ findWithDefault undefined ty (allCons st)

isExhaustive :: [Pattern a] -> PatternM Bool
isExhaustive ps = not <$> useful ps (Wildcard undefined)

isCompleteSet :: [Name a] -> PatternM Bool
isCompleteSet []       = pure False
isCompleteSet ns@(n:_) = do
    allU <- assocUniques n
    let ty = unUnique . unique <$> ns
    pure $ IS.null (allU IS.\\ IS.fromList ty)

useful :: [Pattern a] -> Pattern a -> PatternM Bool
useful ps p = usefulMaranget [[p'] | p' <- ps] [p]

specializeTag :: Name a -> [[Pattern a]] -> [[Pattern a]]
specializeTag c = concatMap withRow
    where withRow (PatternCons _ c':ps) | c' == c = [ps]
                                        | otherwise = []
          withRow (Wildcard{}:ps)       = [ps]

-- TODO: unit test these to make sure they make sense w.r.t. dimensions & such?
specializeTuple :: Int -> [[Pattern a]] -> [[Pattern a]]
specializeTuple n = concatMap withRow
    where withRow (PatternTuple _ ps:ps') = [toList ps ++ ps']
          withRow (p@Wildcard{}:ps')      = [replicate n p ++ ps']
          withRow (OrPattern _ rs:ps)     = [r:ps | r <- toList rs]

-- follows maranget paper
usefulMaranget :: [[Pattern a]] -> [Pattern a] -> PatternM Bool
usefulMaranget [] _                       = pure True
usefulMaranget _ []                       = pure False
usefulMaranget ps (PatternCons _ c:qs)    = usefulMaranget (specializeTag c ps) qs
usefulMaranget ps (PatternTuple _ ps':qs) = usefulMaranget (specializeTuple (length ps') ps) (toList ps' ++ qs)
usefulMaranget ps (OrPattern _ ps':qs)    = forAnyA ps' $ \p -> usefulMaranget ps (p:qs)
