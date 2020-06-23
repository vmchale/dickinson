{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL ( dickinsonRepl
            ) where

import           Control.Monad.Except                  (runExceptT)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.State.Lazy              (StateT, evalStateT, get, gets, lift, put)
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Foldable                         (traverse_)
import qualified Data.Map                              as M
import           Data.Maybe                            (fromJust)
import           Data.Semigroup                        ((<>))
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as TIO
import qualified Data.Text.Lazy                        as TL
import           Data.Text.Lazy.Encoding               (encodeUtf8)
import           Data.Text.Prettyprint.Doc             (Pretty (pretty), hardline)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Data.Tuple.Ext                        (fst3)
import           Language.Dickinson.Eval
import           Language.Dickinson.Lexer              (AlexPosn, AlexUserState, alexInitUserState)
import           Language.Dickinson.Lib
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Lens.Micro                            (_1)
import           Lens.Micro.Mtl                        (use, (.=))
import           REPL.Save
import           System.Console.Haskeline              (InputT, defaultSettings, getInputLine, historyFile, runInputT)
import           System.Directory                      (getHomeDirectory)
import           System.FilePath                       ((</>))
import           System.Random                         (newStdGen, randoms)

dickinsonRepl :: IO ()
dickinsonRepl = runRepl loop

type Repl a = InputT (StateT (EvalSt a) IO)

runRepl :: Repl a x -> IO x
runRepl x = do
    g <- newStdGen
    emdDir <- (</> ".emd_history") <$> getHomeDirectory
    let initSt = EvalSt (randoms g) mempty (initRenames 0) mempty alexInitUserState mempty
    let emdSettings = defaultSettings { historyFile = Just emdDir }
    flip evalStateT initSt $ runInputT emdSettings x

loop :: Repl AlexPosn ()
loop = do
    inp <- getInputLine "emd> "
    case words <$> inp of
        -- TODO: qualified imports?
        Just []                -> loop
        Just (":save":fp:_)    -> saveReplSt fp *> loop
        Just (":l":fs)         -> traverse loadFile fs *> loop
        Just (":load":fs)      -> traverse loadFile fs *> loop
        Just (":r":fp:_)       -> loadReplSt fp *> loop
        Just (":restore":fp:_) -> loadReplSt fp *> loop
        Just [":q"]            -> pure ()
        Just [":quit"]         -> pure ()
        Just [":list"]         -> listNames *> loop
        Just [":dump"]         -> dumpSt *> loop
        -- TODO: erase/delete names?
        Just{}                 -> printExpr (fromJust inp) *> loop
        Nothing                -> pure ()

saveReplSt :: FilePath -> Repl AlexPosn ()
saveReplSt fp = do
    eSt <- lift get
    liftIO $ BSL.writeFile fp (encodeReplSt eSt)

loadReplSt :: FilePath -> Repl AlexPosn ()
loadReplSt fp = do
    contents <- liftIO $ BSL.readFile fp
    g <- liftIO newStdGen
    lift $ put (decodeReplSt (randoms g) contents)

dumpSt :: Repl AlexPosn ()
dumpSt = do
    st <- lift get
    liftIO $ putDoc $ pretty st <> hardline

listNames :: Repl AlexPosn ()
listNames = liftIO . traverse_ TIO.putStrLn =<< names

names :: Repl AlexPosn [T.Text]
names = lift $ gets (M.keys . topLevel)

setSt :: AlexUserState -> Repl AlexPosn ()
setSt newSt = lift $ do
    m' <- use (rename.maxLens)
    let newM = 1 + max (fst3 newSt) m'
    lexerStateLens .= newSt
    lexerStateLens._1 .= newM
    rename.maxLens .= newM

printExpr :: String -> Repl AlexPosn ()
printExpr str = do
    let bsl = encodeUtf8 (TL.pack str)
    aSt <- lift $ gets lexerState
    case parseReplWithCtx bsl aSt of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (newSt, p) -> do
                setSt newSt
                case p of
                    Right expr -> do
                        mErr <- lift $ runExceptT $ evalExpressionAsTextM =<< renameExpressionM expr
                        lift balanceMax
                        putErr mErr (liftIO . TIO.putStrLn)
                    Left decl -> do
                        pathMod <- liftIO defaultLibPath
                        mErr <- lift $ runExceptT $ addDecl (pathMod ["."]) =<< renameDeclarationM decl
                        lift balanceMax
                        putErr mErr (const $ pure ())

putErr :: Pretty e => Either e b -> (b -> Repl a ()) -> Repl a ()
putErr (Right x) f = f x
putErr (Left y) _  = liftIO $ putDoc (pretty y <> hardline)

-- TODO: check
loadFile :: FilePath -> Repl AlexPosn ()
loadFile fp = do
    contents <- liftIO $ BSL.readFile fp
    preSt <- lift $ gets lexerState
    case parseWithCtx contents preSt of
        Left err     -> liftIO $ putDoc (pretty err)
        Right (newSt, p) -> do
            setSt newSt
            pathMod <- liftIO defaultLibPath
            mErr <- lift $ runExceptT $ loadDickinson (pathMod ["."]) =<< renameDickinsonM p
            lift balanceMax
            putErr mErr (const $ pure ())
