module Language.Dickinson.Check.Internal ( sanityCheck
                                         , sanityCheckLexer
                                         , maxUniqueDeclaration
                                         ) where

import           Control.Monad             (when)
import           Control.Monad.State       (MonadState)
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
maxUniqueExpression Literal{}                             = 0
maxUniqueExpression (Constructor _ (Name _ (Unique k) _)) = k
maxUniqueExpression StrChunk{}                            = 0
maxUniqueExpression (Var _ (Name _ (Unique i) _))         = i
maxUniqueExpression (Choice _ pes)                        = maximum (maxUniqueExpression . snd <$> pes)
maxUniqueExpression (MultiInterp _ es)                    = maximum (fmap maxUniqueExpression es)
maxUniqueExpression (Interp _ es)                         = maximum (fmap maxUniqueExpression es)
maxUniqueExpression (Concat _ es)                         = maximum (fmap maxUniqueExpression es)
maxUniqueExpression (Apply _ e e')                        = max (maxUniqueExpression e) (maxUniqueExpression e')
maxUniqueExpression (Annot _ e ty)                        = max (maxUniqueExpression e) (maxUniqueType ty)
maxUniqueExpression (Flatten _ e)                         = maxUniqueExpression e
maxUniqueExpression (Tuple _ es)                          = maximum (fmap maxUniqueExpression es)
maxUniqueExpression (Lambda _ (Name _ (Unique i) _) ty e) =
    maximum [ i
            , maxUniqueExpression e
            , maxUniqueType ty
            ]
maxUniqueExpression (Match _ e p e')                      =
    maximum
        [ maxUniqueExpression e
        , maxUniquePattern p
        , maxUniqueExpression e'
        ]
maxUniqueExpression (Let _ bs e) =
    maximum
        [ maxUniqueExpression e
        , maximum (maxUniqueExpression . snd <$> bs)
        , maximum (unUnique . unique . fst <$> bs)
        ]

maxUniquePattern :: Pattern a -> Int
maxUniquePattern (PatternVar _ (Name _ (Unique i) _))  = i
maxUniquePattern Wildcard{}                            = 0
maxUniquePattern (PatternTuple _ ps)                   = maximum (fmap maxUniquePattern ps)
maxUniquePattern (PatternCons _ (Name _ (Unique k) _)) = k
