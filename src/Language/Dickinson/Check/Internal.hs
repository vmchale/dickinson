module Language.Dickinson.Check.Internal ( sanityCheck
                                         ) where

import           Control.Monad             (when)
import           Control.Monad.State       (MonadState)
import           Language.Dickinson.Name
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import           Language.Dickinson.Unique
import           Lens.Micro.Mtl            (use)

-- TODO: sanity check for the lexer
sanityCheck :: (HasRenames s, MonadState s m) => [Declaration a] -> m ()
sanityCheck d = do
    storedMax <- use (rename.maxLens)
    let computedMax = maximum (maxUniqueDeclaration <$> d)
    when (storedMax < computedMax) $
        error "Sanity check failed!"

maxUniqueDeclaration :: Declaration a -> Int
maxUniqueDeclaration (Define _ (Name _ (Unique i) _) e) = max i (maxUniqueExpression e)

maxUniqueExpression :: Expression a -> Int
maxUniqueExpression Literal{}                             = 0
maxUniqueExpression (Constructor _ (Name _ (Unique k) _)) = k
maxUniqueExpression StrChunk{}                            = 0
maxUniqueExpression (Var _ (Name _ (Unique i) _))         = i
maxUniqueExpression (Choice _ pes)                        = maximum (maxUniqueExpression . snd <$> pes)
maxUniqueExpression (Interp _ es)                         = maximum (fmap maxUniqueExpression es)
maxUniqueExpression (Concat _ es)                         = maximum (fmap maxUniqueExpression es)
maxUniqueExpression (Apply _ e e')                        = max (maxUniqueExpression e) (maxUniqueExpression e')
maxUniqueExpression (Annot _ e _)                         = maxUniqueExpression e
maxUniqueExpression (Flatten _ e)                         = maxUniqueExpression e
maxUniqueExpression (Tuple _ es)                          = maximum (fmap maxUniqueExpression es)
maxUniqueExpression (Lambda _ (Name _ (Unique i) _) _ e)  = max i (maxUniqueExpression e)
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
