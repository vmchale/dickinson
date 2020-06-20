{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Eval ( EvalM
                               , EvalSt (..)
                               , addDecl
                               , loadDickinson
                               , evalDickinsonAsMain
                               , evalExpressionM
                               , normalizeExpressionM
                               , evalWithGen
                               , evalIO
                               , findDecl
                               , findMain
                               , lexerStateLens
                               , balanceMax
                               ) where

import           Control.Monad                         (zipWithM_, (<=<))
import           Control.Monad.Except                  (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class                (MonadIO (..))
import           Control.Monad.State.Lazy              (MonadState, StateT, evalStateT, get, gets, modify)
import           Data.Foldable                         (toList, traverse_)
import qualified Data.IntMap                           as IM
import           Data.List.NonEmpty                    (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (Doc, Pretty (..), vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Ext
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Language.Dickinson.Error
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck
import           Language.Dickinson.Unique
import           Lens.Micro                            (Lens', over, _1)
import           Lens.Micro.Mtl                        (use, (.=))
import           System.Random                         (StdGen, newStdGen, randoms)

-- | The state during evaluation
data EvalSt a = EvalSt
    { probabilities :: [Double]
    -- map to expression
    , boundExpr     :: IM.IntMap (Expression a)
    , renameCtx     :: Renames
    -- TODO: map to uniques or an expression?
    -- TODO: module context for topLevel?
    , topLevel      :: M.Map T.Text Unique
    -- For imports & such.
    , lexerState    :: AlexUserState
    -- For error messages
    , tyEnv         :: TyEnv
    }

lexerStateLens :: Lens' (EvalSt a) AlexUserState
lexerStateLens f s = fmap (\x -> s { lexerState = x }) (f (lexerState s))

prettyBound :: (Int, Expression a) -> Doc b
prettyBound (i, e) = pretty i <+> "←" <#*> pretty e

prettyTl :: (T.Text, Unique) -> Doc a
prettyTl (t, i) = pretty t <+> ":" <+> pretty i

instance Pretty (EvalSt a) where
    pretty (EvalSt _ b r t st _) =
        "bound expressions:" <#> vsep (prettyBound <$> IM.toList b)
            <#> pretty r
            <#> "top-level names:" <#> vsep (prettyTl <$> M.toList t)
            <#> prettyAlexState st

prettyAlexState :: AlexUserState -> Doc a
prettyAlexState (m, _, nEnv) = "max:" <+> pretty m <#> prettyDumpBinds nEnv

instance HasRenames (EvalSt a) where
    rename f s = fmap (\x -> s { renameCtx = x }) (f (renameCtx s))

instance HasTyEnv (EvalSt a) where
    tyEnvLens f s = fmap (\x -> s { tyEnv = x }) (f (tyEnv s))

probabilitiesLens :: Lens' (EvalSt a) [Double]
probabilitiesLens f s = fmap (\x -> s { probabilities = x }) (f (probabilities s))

boundExprLens :: Lens' (EvalSt a) (IM.IntMap (Expression a))
boundExprLens f s = fmap (\x -> s { boundExpr = x }) (f (boundExpr s))

topLevelLens :: Lens' (EvalSt a) (M.Map T.Text Unique)
topLevelLens f s = fmap (\x -> s { topLevel = x }) (f (topLevel s))

-- TODO: thread generator state instead?
type EvalM a = StateT (EvalSt a) (ExceptT (DickinsonError a) IO)

evalIO :: AlexUserState -> EvalM a x -> IO (Either (DickinsonError a) x)
evalIO rs me = (\g -> evalWithGen g rs me) =<< newStdGen

evalWithGen :: StdGen
            -> AlexUserState -- ^ Threaded through
            -> EvalM a x
            -> IO (Either (DickinsonError a) x)
evalWithGen g u me = runExceptT $ evalStateT me (EvalSt (randoms g) mempty (initRenames $ fst3 u) mempty u mempty)
    where fst3 (x, _, _) = x

-- TODO: temporary bindings
bindName :: (MonadState (EvalSt a) m) => Name a -> Expression a -> m ()
bindName (Name _ (Unique u) _) e = modify (over boundExprLens (IM.insert u e))
-- TODO: bind type information

topLevelAdd :: (MonadState (EvalSt a) m) => Name a -> m ()
topLevelAdd (Name (n :| []) u _) = modify (over topLevelLens (M.insert n u))
topLevelAdd Name{}               = error "Top-level names cannot be qualified"

deleteName :: (MonadState (EvalSt a) m) => Name a -> m ()
deleteName (Name _ (Unique u) _) = modify (over boundExprLens (IM.delete u))

lookupName :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Name a -> m (Expression a)
lookupName n@(Name _ (Unique u) l) = go =<< gets (IM.lookup u.boundExpr)
    where go Nothing  = throwError (UnfoundName l n)
          go (Just x) = renameExpressionM x

normalize :: (Foldable t, Functor t, Fractional a) => t a -> t a
normalize xs = {-# SCC "normalize" #-} (/tot) <$> xs
    where tot = sum xs

cdf :: (Num a) => NonEmpty a -> [a]
cdf = {-# SCC "cdf" #-} NE.drop 2 . NE.scanl (+) 0 . (0 <|)

pick :: (MonadState (EvalSt a) m) => NonEmpty (Double, Expression a) -> m (Expression a)
pick brs = {-# SCC "pick" #-} do
    threshold <- gets (head.probabilities)
    modify (over probabilitiesLens tail)
    let ds = cdf (normalize (fst <$> brs))
        es = toList (snd <$> brs)
    pure $ snd . head . dropWhile ((<= threshold) . fst) $ zip ds es

findDecl :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => T.Text -> m (Expression a)
findDecl t = do
    tops <- gets topLevel
    case M.lookup t tops of
        Just (Unique i) -> do { es <- gets boundExpr ; pure (es IM.! i) }
        Nothing         -> throwError (NoText t)

findMain :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => m (Expression a)
findMain = findDecl "main"

evalDickinsonAsMain :: (MonadError (DickinsonError AlexPosn) m, MonadState (EvalSt AlexPosn) m) => Dickinson AlexPosn -> m T.Text
evalDickinsonAsMain d =
    loadDickinson d *>
    (evalExpressionM =<< findMain)

loadDickinson :: (MonadError (DickinsonError AlexPosn) m, MonadState (EvalSt AlexPosn) m) => Dickinson AlexPosn -> m ()
loadDickinson = traverse_ addDecl

balanceMax :: MonadState (EvalSt a) m => m ()
balanceMax = do
    m0 <- use (rename.maxLens)
    m1 <- use (lexerStateLens._1)
    let m' = max m0 m1
    rename.maxLens .= m'
    lexerStateLens._1 .= m'

debugSt :: (MonadIO m, MonadState (EvalSt a) m) => m ()
debugSt = do
    st <- get
    liftIO $ putDoc (pretty st)

-- TODO: MonadIO to addDecl so can import
addDecl :: (MonadError (DickinsonError AlexPosn) m, MonadState (EvalSt AlexPosn) m) => Declaration AlexPosn -> m ()
addDecl (Define _ n e) = bindName n e *> topLevelAdd n

extrText :: (HasTyEnv s, MonadState s m, MonadError (DickinsonError a) m) => Expression a -> m T.Text
extrText (Literal _ t)  = pure t
extrText (StrChunk _ t) = pure t
extrText e              = do { ty <- typeOf e ; throwError $ TypeMismatch e TyText ty }

bindPattern :: (MonadError (DickinsonError a) m, MonadState (EvalSt a) m) => Pattern a -> Expression a -> m ()
bindPattern (PatternVar _ n) e                       = bindName n e
bindPattern Wildcard{} _                             = pure ()
bindPattern (PatternTuple _ []) (Tuple _ [])         = pure ()
bindPattern (PatternTuple _ (p:ps)) (Tuple _ (e:es)) = bindPattern p e *> zipWithM_ bindPattern ps es
bindPattern (PatternTuple l _) _                     = throwError $ MalformedTuple l

normalizeExpressionM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Expression a -> m (Expression a)
normalizeExpressionM e@Literal{}    = pure e
normalizeExpressionM e@StrChunk{}   = pure e
normalizeExpressionM (Var _ n)      = normalizeExpressionM =<< lookupName n
normalizeExpressionM (Choice _ pes) = normalizeExpressionM =<< pick pes
-- FIXME: this is overzealous I think...
normalizeExpressionM (Interp l es)  = concatOrFail l es
normalizeExpressionM (Concat l es)  = concatOrFail l es
normalizeExpressionM (Tuple l es)   = Tuple l <$> traverse normalizeExpressionM es
normalizeExpressionM (Let _ bs e) = do
    traverse_ (uncurry bindName) bs
    normalizeExpressionM e <* traverse_ deleteName (fst <$> bs)
    -- FIXME: assumes global uniqueness in renaming
normalizeExpressionM (Apply _ e e') = do
    e'' <- normalizeExpressionM e
    case e'' of
        Lambda _ n _ e''' -> do
            bindName n e'
            normalizeExpressionM e'''
        _ -> error "Ill-typed expression"
normalizeExpressionM e@Lambda{} = pure e
normalizeExpressionM (Match _ e p e') =
    (bindPattern p =<< normalizeExpressionM e) *>
    normalizeExpressionM e'

concatOrFail :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => a -> [Expression a] -> m (Expression a)
concatOrFail l = fmap (Literal l . mconcat) . traverse evalExpressionM

evalExpressionM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Expression a -> m T.Text
evalExpressionM = extrText <=< normalizeExpressionM
