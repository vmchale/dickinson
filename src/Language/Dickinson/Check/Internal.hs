module Language.Dickinson.Check.Internal ( sanityCheck
                                         , sanityCheckLexer
                                         , maxUniqueDeclaration
                                         ) where

import           Control.Monad             (when)
import           Control.Monad.State       (MonadState)
import           Control.Recursion         (cata)
import           Data.List.NonEmpty        ((<|))
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro                (_1)
import           Lens.Micro.Mtl            (use)

sanityCheckLexer :: (HasLexerState s, MonadState s m) => [Declaration a] -> m ()
sanityCheckLexer d = do
    storedMax <- use (lexerStateLens._1)
    let computedMax = maximum (maxUniqueDeclaration <$> d)
    when (storedMax < computedMax) $
        error "Sanity check failed!"

-- | Sanity check for the renamer.
sanityCheck :: (HasRenames s, MonadState s m) => [Declaration a] -> m ()
sanityCheck d = do
    storedMax <- use (rename.maxLens)
    let computedMax = maximum (maxUniqueDeclaration <$> d)
    when (storedMax < computedMax) $
        error "Sanity check failed!"

-- TODO: see http://hackage.haskell.org/package/uniplate-1.6.12/docs/Data-Generics-Uniplate-Operations.html
-- TODO: recursion schemes? would need benchmark...
maxUniqueDeclaration :: Declaration a -> Int
maxUniqueDeclaration (Define _ (Name _ (Unique i) _) e)   = max i (maxUniqueExpression e)
maxUniqueDeclaration (TyDecl _ (Name _ (Unique i) _) tns) =
    maximum ( i <| fmap (unUnique . unique) tns)

maxUniqueType :: DickinsonTy a -> Int
maxUniqueType TyText{}                          = 0
maxUniqueType (TyFun _ ty ty')                  = max (maxUniqueType ty) (maxUniqueType ty')
maxUniqueType (TyTuple _ ts)                    = maximum (fmap maxUniqueType ts)
maxUniqueType (TyNamed _ (Name _ (Unique k) _)) = k

maxUniqueExpression :: Expression a -> Int
maxUniqueExpression = cata a where
    a LiteralF{}                              = 0
    a (ConstructorF _ (Name _ (Unique k) _))  = k
    a StrChunkF{}                             = 0
    a (VarF _ (Name _ (Unique i) _))          = i
    a (ChoiceF _ pes)                         = maximum (snd <$> pes)
    a (MultiInterpF _ es)                     = maximum es
    a (InterpF _ es)                          = maximum es
    a (ConcatF _ es)                          = maximum es
    a (ApplyF _ e e')                         = max e e'
    a (AnnotF _ e ty)                         = max e (maxUniqueType ty)
    a (FlattenF _ e)                          = e
    a (TupleF _ es)                           = maximum es
    a (LambdaF  _ (Name _ (Unique i) _) ty e) = maximum [i, e, maxUniqueType ty]
    a (MatchF _ e p e')                       = maximum [e, maxUniquePattern p, e']
    a (LetF _ bs e)                           = maximum [e, maximum (snd <$> bs), maximum (unUnique . unique . fst <$> bs)]

maxUniquePattern :: Pattern a -> Int
maxUniquePattern = cata a where
    a (PatternVarF _ (Name _ (Unique i) _))  = i
    a WildcardF{}                            = 0
    a (PatternTupleF _ ps)                   = maximum ps
    a (PatternConsF _ (Name _ (Unique k) _)) = k
