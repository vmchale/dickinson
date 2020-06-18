{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL ( dickinsonRepl
            ) where

import           Control.Monad.Except                  (runExceptT)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.State.Lazy              (StateT, evalStateT, get, gets, lift)
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Foldable                         (traverse_)
import qualified Data.Map                              as M
import           Data.Maybe                            (fromJust)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as TIO
import qualified Data.Text.Lazy                        as TL
import           Data.Text.Lazy.Encoding               (encodeUtf8)
import           Data.Text.Prettyprint.Doc             (Pretty (pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Language.Dickinson
import           Lens.Micro                            (_1)
import           Lens.Micro.Mtl                        (use, (.=))
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
    let initSt = EvalSt (randoms g) mempty (initRenames 0) mempty alexInitUserState
    let emdSettings = defaultSettings { historyFile = Just emdDir }
    flip evalStateT initSt $ runInputT emdSettings x

loop :: Repl AlexPosn ()
loop = do
    inp <- getInputLine "emd> "
    case words <$> inp of
        -- TODO: qualified imports?
        Just []        -> loop
        Just (":l":fs) -> traverse loadFile fs *> loop
        Just [":q"]    -> pure ()
        Just [":list"] -> listNames *> loop
        Just [":dump"] -> dumpSt *> loop
        -- FIXME: expression renames?
        -- lexer has no context...
        Just{}         -> printExpr (fromJust inp) *> loop
        Nothing        -> pure ()

dumpSt :: Repl AlexPosn ()
dumpSt = do
    st <- lift get
    liftIO $ putDoc $ pretty st

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

    where fst3 (x, _, _) = x

printExpr :: String -> Repl AlexPosn ()
printExpr str = do
    let bsl = encodeUtf8 (TL.pack str)
    aSt <- lift $ gets lexerState
    case parseExpressionWithCtx bsl aSt of
        Left err -> liftIO $ putDoc (pretty err)
        Right (newSt, p) -> do
                setSt newSt
                mErr <- lift $ runExceptT $ evalExpressionM =<< renameExpressionM p
                lift balanceMax
                putErr mErr (liftIO . TIO.putStrLn)

putErr :: Pretty e => Either e b -> (b -> Repl a ()) -> Repl a ()
putErr (Right x) f = f x
putErr (Left y) _  = liftIO $ putDoc (pretty y)

-- TODO: check
loadFile :: FilePath -> Repl AlexPosn ()
loadFile fp = do
    contents <- liftIO $ BSL.readFile fp
    preSt <- lift $ gets lexerState
    case parseWithCtx contents preSt of
        Left err     -> liftIO $ putDoc (pretty err)
        Right (newSt, p) -> do
            setSt newSt
            mErr <- lift $ runExceptT $ loadDickinson =<< renameDickinsonM p
            lift balanceMax
            putErr mErr (const $ pure ())
