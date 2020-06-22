{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Dickinson.Eval ( EvalM
                               , EvalSt (..)
                               , addDecl
                               , loadDickinson
                               , evalDickinsonAsMain
                               , evalExpressionM
                               , evalExpressionAsTextM
                               , evalWithGen
                               , evalIO
                               , findDecl
                               , findMain
                               , lexerStateLens
                               , balanceMax
                               ) where

import           Control.Composition           (thread)
import           Control.Monad                 (zipWithM, zipWithM_, (<=<))
import           Control.Monad.Except          (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.State.Lazy      (MonadState, StateT, evalStateT, get, gets, modify, put)
import qualified Data.ByteString.Lazy          as BSL
import           Data.Foldable                 (toList, traverse_)
import           Data.Functor                  (($>))
import qualified Data.IntMap                   as IM
import           Data.List.NonEmpty            (NonEmpty, (<|))
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (Doc, Pretty (..), vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Ext
import           Data.Tuple.Ext
import           Language.Dickinson.Error
import           Language.Dickinson.Import
import           Language.Dickinson.Lexer
import           Language.Dickinson.Name
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck
import           Language.Dickinson.Unique
import           Lens.Micro                    (Lens', over, _1)
import           Lens.Micro.Mtl                (use, (.=))
import           System.Random                 (StdGen, newStdGen, randoms)

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
prettyAlexState (m, _, nEnv) =
        "max:" <+> pretty m
    <#> prettyDumpBinds nEnv

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

nameMod :: Name a -> Expression a -> EvalSt a -> EvalSt a
nameMod (Name _ (Unique u) _) e = over boundExprLens (IM.insert u e)

bindName :: (MonadState (EvalSt a) m) => Name a -> Expression a -> m ()
bindName n e = modify (nameMod n e)

topLevelAdd :: (MonadState (EvalSt a) m) => Name a -> m ()
topLevelAdd (Name n u _) = modify (over topLevelLens (M.insert (T.intercalate "." $ toList n) u))

lookupName :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Name a -> m (Expression a)
lookupName n@(Name _ u l) = do
    (Unique u') <- replaceUnique u
    go =<< gets (IM.lookup u'.boundExpr)
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

evalDickinsonAsMain :: (MonadError (DickinsonError AlexPosn) m, MonadState (EvalSt AlexPosn) m, MonadIO m)
                    => [FilePath]
                    -> Dickinson AlexPosn
                    -> m T.Text
evalDickinsonAsMain is d =
    loadDickinson is d *>
    (evalExpressionAsTextM =<< findMain)

loadDickinson :: (MonadError (DickinsonError AlexPosn) m, MonadState (EvalSt AlexPosn) m, MonadIO m)
              => [FilePath] -- ^ Include path
              -> Dickinson AlexPosn
              -> m ()
loadDickinson is = traverse_ (addDecl is)

balanceMax :: MonadState (EvalSt a) m => m ()
balanceMax = do
    m0 <- use (rename.maxLens)
    m1 <- use (lexerStateLens._1)
    let m' = max m0 m1
    rename.maxLens .= m'
    lexerStateLens._1 .= m'

parseEvalM :: (MonadIO m, MonadState (EvalSt AlexPosn) m, MonadError (DickinsonError AlexPosn) m)
           => FilePath
           -> m (Dickinson AlexPosn)
parseEvalM fp = do
    preSt <- gets lexerState
    bsl <- liftIO $ BSL.readFile fp
    case parseWithCtx bsl preSt of
        Right (st, d) ->
            (lexerStateLens .= st) $> d
        Left err ->
            throwError (ParseErr err)

-- TODO: MonadIO to addDecl so can import
addDecl :: (MonadError (DickinsonError AlexPosn) m, MonadState (EvalSt AlexPosn) m, MonadIO m)
        => [FilePath] -- ^ Include path
        -> Declaration AlexPosn
        -> m ()
addDecl _ (Define _ n e) = bindName n e *> topLevelAdd n
addDecl is (Import l n)  = do
    preFp <- resolveImport is n
    case preFp of
        Just fp -> do
            parsed <- parseEvalM fp
            balanceMax
            renamed <- renameDickinsonM parsed
            loadDickinson is renamed
        Nothing -> throwError $ ModuleNotFound l n

extrText :: (HasTyEnv s, MonadState s m, MonadError (DickinsonError a) m) => Expression a -> m T.Text
extrText (Literal _ t)  = pure t
extrText (StrChunk _ t) = pure t
extrText e              = do { ty <- typeOf e ; throwError $ TypeMismatch e TyText ty }

withSt :: (MonadState s m) => (s -> s) -> m b -> m b
withSt modSt act = do
    preSt <- get
    modify modSt
    act <* (put preSt)

bindPattern :: (MonadError (DickinsonError a) m, MonadState (EvalSt a) m) => Pattern a -> Expression a -> m (EvalSt a -> EvalSt a)
bindPattern (PatternVar _ n) e               = pure $ nameMod n e
bindPattern Wildcard{} _                     = pure id
bindPattern (PatternTuple _ ps) (Tuple _ es) = thread <$> zipWithM bindPattern ps es
bindPattern (PatternTuple l _) _             = throwError $ MalformedTuple l

evalExpressionM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Expression a -> m (Expression a)
evalExpressionM e@Literal{}    = pure e
evalExpressionM e@StrChunk{}   = pure e
evalExpressionM (Var _ n)      = evalExpressionM =<< lookupName n
evalExpressionM (Choice _ pes) = evalExpressionM =<< pick pes
-- FIXME: this is overzealous I think...
evalExpressionM (Interp l es)  = concatOrFail l es
evalExpressionM (Concat l es)  = concatOrFail l es
evalExpressionM (Tuple l es)   = Tuple l <$> traverse evalExpressionM es
evalExpressionM (Let _ bs e) = do
    es' <- traverse evalExpressionM (snd <$> bs)
    let ns = fst <$> bs
        newBs = NE.zip ns es'
    traverse_ (uncurry bindName) newBs
    let stMod = thread $ fmap (uncurry nameMod) newBs
    withSt stMod $
        evalExpressionM e
evalExpressionM (Apply _ e e') = do
    e'' <- evalExpressionM e
    case e'' of
        Lambda _ n _ e''' ->
            withSt (nameMod n e') $
                evalExpressionM e'''
        _ -> error "Ill-typed expression"
evalExpressionM e@Lambda{} = pure e
evalExpressionM (Match _ e p e') = do
    modSt <- (bindPattern p =<< evalExpressionM e)
    -- FIXME: this evaluates 'pick' too zealously in repls?
    -- maybe has to do with global uniqueness lol
    withSt modSt $
        evalExpressionM e'

concatOrFail :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => a -> [Expression a] -> m (Expression a)
concatOrFail l = fmap (Literal l . mconcat) . traverse evalExpressionAsTextM

evalExpressionAsTextM :: (MonadState (EvalSt a) m, MonadError (DickinsonError a) m) => Expression a -> m T.Text
evalExpressionAsTextM = extrText <=< evalExpressionM
