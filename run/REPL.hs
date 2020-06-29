{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL ( dickinsonRepl
            ) where

import           Control.Monad.Except                  (ExceptT, runExceptT)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.State.Lazy              (StateT, evalStateT, get, gets, lift, put)
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Foldable                         (traverse_)
import qualified Data.IntMap                           as IM
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
import           Language.Dickinson.Error
import           Language.Dickinson.Eval
import           Language.Dickinson.File
import           Language.Dickinson.Lexer              (AlexPosn, AlexUserState, alexInitUserState)
import           Language.Dickinson.Lib
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Language.Dickinson.ScopeCheck
import           Language.Dickinson.Type
import           Language.Dickinson.TypeCheck
import           Language.Dickinson.Unique
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
    let initSt = EvalSt (randoms g) mempty initRenames mempty alexInitUserState emptyTyEnv
    let emdSettings = defaultSettings { historyFile = Just emdDir }
    flip evalStateT initSt $ runInputT emdSettings x

loop :: Repl AlexPosn ()
loop = do
    inp <- getInputLine "emd> "
    case words <$> inp of
        Just []             -> loop
        Just (":h":_)       -> showHelp *> loop
        Just (":help":_)    -> showHelp *> loop
        Just (":save":fp:_) -> saveReplSt fp *> loop
        Just (":l":fs)      -> traverse loadFile fs *> loop
        Just (":load":fs)   -> traverse loadFile fs *> loop
        Just (":r":fp:_)    -> loadReplSt fp *> loop
        Just (":type":e:_)  -> typeExpr e *> loop
        Just (":t":e:_)     -> typeExpr e *> loop
        Just (":v":n:_)     -> bindDisplay (T.pack n) *> loop
        Just (":view":n:_)  -> bindDisplay (T.pack n) *> loop
        Just [":q"]         -> pure ()
        Just [":quit"]      -> pure ()
        Just [":list"]      -> listNames *> loop
        Just [":dump"]      -> dumpSt *> loop
        -- TODO: erase/delete names?
        Just{}              -> printExpr (fromJust inp) *> loop
        Nothing             -> pure ()

rightPad :: Int -> String -> String
rightPad n str = take n $ str ++ repeat ' '

showHelp :: Repl AlexPosn ()
showHelp = liftIO $ putStr $ concat
    [ helpOption ":help, :h" "" "Show this help"
    , helpOption ":save" "<file>" "Save current state"
    , helpOption ":load, :l" "<file>" "Load file contents"
    , helpOption ":r" "<file>" "Restore REPL state from a file"
    , helpOption ":type, :t" "<expression>" "Display the type of an expression"
    , helpOption ":view, :v" "<name>" "Show the value of a name"
    , helpOption ":quite, :q" "" "Quit REPL"
    , helpOption ":list" "" "List all names that are in scope"
    ]

helpOption :: String -> String -> String -> String
helpOption cmd args desc =
    rightPad 15 cmd ++ rightPad 14 args ++ desc ++ "\n"

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

bindDisplay :: T.Text -> Repl AlexPosn ()
bindDisplay t = do
    preBinds <- lift $ gets topLevel
    let u = M.lookup t preBinds
    case u of
        Just (Unique i) -> do
            exprs <- lift $ gets boundExpr
            case IM.lookup i exprs of
                Just e  -> liftIO $ putDoc (pretty e <> hardline)
                Nothing -> error "Internal error."
        Nothing -> pure () -- TODO: error

setSt :: AlexUserState -> Repl AlexPosn ()
setSt newSt = lift $ do
    m' <- use (rename.maxLens)
    let newM = 1 + max (fst3 newSt) m'
    lexerStateLens .= newSt
    lexerStateLens._1 .= newM
    rename.maxLens .= newM

strBytes :: String -> BSL.ByteString
strBytes = encodeUtf8 . TL.pack

typeExpr :: String -> Repl AlexPosn ()
typeExpr str = do
    let bsl = strBytes str
    aSt <- lift $ gets lexerState
    case parseExpressionWithCtx bsl aSt of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (newSt, e) -> do
            setSt newSt
            mErr <- lift $ runExceptT $ typeOf =<< resolveExpressionM =<< renameExpressionM e
            lift balanceMax
            putErr mErr (liftIO . putDoc . (<> hardline) . pretty)

printExpr :: String -> Repl AlexPosn ()
printExpr str = do
    let bsl = strBytes str
    aSt <- lift $ gets lexerState
    case parseReplWithCtx bsl aSt of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (newSt, p) -> do
                setSt newSt
                case p of
                    Right e -> do
                        mErr <- lift $ runExceptT $ do
                            e' <- resolveExpressionM =<< renameExpressionM e
                            checkScopeExpr e'
                            evalExpressionAsTextM e'
                        lift balanceMax
                        putErr mErr (liftIO . TIO.putStrLn)
                    Left decl -> do
                        mErr <- lift $ runExceptT $ do
                            d <- renameDeclarationM decl
                            checkScopeDecl =<< resolveDeclarationM d
                            addDecl' d
                        lift balanceMax
                        putErr mErr (const $ pure ())

    where addDecl' :: Declaration AlexPosn -> ExceptT (DickinsonError AlexPosn) (StateT (EvalSt AlexPosn) IO) ()
          addDecl' = addDecl


putErr :: Pretty e => Either e b -> (b -> Repl a ()) -> Repl a ()
putErr (Right x) f = f x
putErr (Left y) _  = liftIO $ putDoc (pretty y <> hardline)

-- TODO: check
loadFile :: FilePath -> Repl AlexPosn ()
loadFile fp = do
    pathMod <- liftIO defaultLibPath
    mErr <- lift $ runExceptT $ do
        d <- amalgamateRenameM (pathMod ["."]) fp
        maybeThrow $ checkScope d
        loadDickinson d
    lift balanceMax
    putErr mErr (const $ pure ())
