module Roundtrip.Compare ( compareDickinson
                         ) where

import           Data.List.NonEmpty      (NonEmpty (..))
import           Language.Dickinson.Name
import           Language.Dickinson.Type

compareDickinson :: Dickinson a -> Dickinson a -> Bool
compareDickinson (Dickinson is ds) (Dickinson is' ds') =
    compareList compareImport is is'
        && compareList compareDeclaration ds ds'

compareDeclaration :: Declaration a -> Declaration a -> Bool
compareDeclaration (TyDecl _ n tns) (TyDecl _ n' tns') = compareName n n' && compareNonEmpty compareName tns tns'
compareDeclaration (Define _ n e) (Define _ n' e')     = compareName n n' && compareExpression e e'
compareDeclaration _ _                                 = False

compareType :: DickinsonTy a -> DickinsonTy a -> Bool
compareType TyText{} TyText{}                     = True
compareType (TyFun _ ty ty') (TyFun _ ty'' ty''') = compareType ty ty'' && compareType ty' ty'''
compareType (TyNamed _ n) (TyNamed _ n')          = compareName n n'
compareType (TyTuple _ tys) (TyTuple _ tys')      = compareNonEmpty compareType tys tys'
compareType _ _                                   = False

compareExpression :: Expression a -> Expression a -> Bool
compareExpression (Literal _ t) (Literal _ t')           = t == t'
compareExpression (StrChunk _ t) (StrChunk _ t')         = t == t'
compareExpression (Choice _ brs) (Choice _ brs')         = compareNonEmpty (\(p, e) (p', e') -> p == p' && compareExpression e e') brs brs'
compareExpression (Let _ ls e) (Let _ ls' e')            = compareExpression e e' && compareNonEmpty (\(n, e'') (n', e''') -> compareName n n' && compareExpression e'' e''') ls ls'
compareExpression (Var _ n) (Var _ n')                   = compareName n n'
compareExpression (Interp _ es) (Interp _ es')           = compareList compareExpression es es'
compareExpression (MultiInterp _ es) (MultiInterp _ es') = compareList compareExpression es es'
compareExpression (Lambda _ n ty e) (Lambda _ n' ty' e') = compareName n n' && compareType ty ty' && compareExpression e e'
compareExpression (Apply _ e e') (Apply _ e'' e''')      = compareExpression e e'' && compareExpression e' e'''
compareExpression (Concat _ es) (Concat _ es')           = compareList compareExpression es es'
compareExpression (Tuple _ es) (Tuple _ es')             = compareNonEmpty compareExpression es es'
compareExpression (Match _ e br) (Match _ e' br')        = compareExpression e e' && compareNonEmpty (\(p, e'') (p', e''') -> comparePattern p p' && compareExpression e'' e''') br br'
compareExpression (Flatten _ e) (Flatten _ e')           = compareExpression e e'
compareExpression (Annot _ e ty) (Annot _ e' ty')        = compareExpression e e' && compareType ty ty'
compareExpression (Constructor _ tn) (Constructor _ tn') = compareName tn tn'
compareExpression (BuiltinFn _ b) (BuiltinFn _ b')       = b == b'
compareExpression _ _                                    = False

compareImport :: Import a -> Import a -> Bool
compareImport (Import _ n) (Import _ n') = compareName n n'

comparePattern :: Pattern a -> Pattern a -> Bool
comparePattern (PatternVar _ n) (PatternVar _ n')       = compareName n n'
comparePattern (PatternTuple _ ps) (PatternTuple _ ps') = compareNonEmpty comparePattern ps ps'
comparePattern (PatternCons _ tn) (PatternCons _ tn')   = compareName tn tn'
comparePattern Wildcard{} Wildcard{}                    = True
comparePattern (OrPattern _ ps) (OrPattern _ ps')       = compareNonEmpty comparePattern ps ps'
comparePattern _ _                                      = False

compareNonEmpty :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> Bool
compareNonEmpty eq (x :| xs) (y :| ys) = x `eq` y && compareList eq xs ys

compareList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
compareList _ [] []          = True
compareList eq (x:xs) (y:ys) = x `eq` y && compareList eq xs ys
compareList _ _ _            = False

compareName :: Name a -> Name a -> Bool
compareName (Name n _ _) (Name n' _ _) = n == n'
