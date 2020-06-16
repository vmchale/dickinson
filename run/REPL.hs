{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module REPL ( dickinsonRepl
            ) where

import           Control.Monad.Except     (runExceptT)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (StateT, evalStateT, get, gets, lift)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Foldable            (traverse_)
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified Data.Text.Lazy           as TL
import           Data.Text.Lazy.Encoding  (encodeUtf8)
import           Language.Dickinson
import           Lens.Micro.Mtl           (modifying)
import           System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import           System.Random            (newStdGen, randoms)

dickinsonRepl :: IO ()
dickinsonRepl = runRepl loop

type Repl a = InputT (StateT (EvalSt a) IO)

runRepl :: Repl a x -> IO x
runRepl x = do
    g <- newStdGen
    let initSt = EvalSt (randoms g) mempty (initRenames 0) mempty
    flip evalStateT initSt $ runInputT defaultSettings x

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

-- TODO: dump EvalSt?
dumpSt :: Repl AlexPosn ()
dumpSt = do
    st <- lift get
    liftIO $ putDoc $ pretty st

listNames :: Repl AlexPosn ()
listNames = liftIO . traverse_ TIO.putStrLn =<< names

names :: Repl AlexPosn [T.Text]
names = lift $ gets (M.keys . topLevel)

-- TODO: check
printExpr :: String -> Repl AlexPosn ()
printExpr str =
    let bsl = encodeUtf8 (TL.pack str)
        in case parseExpressionWithCtx bsl of
            Left err -> liftIO $ putDoc (pretty err)
            Right (m, p) -> lift $ do
                    modifying (rename.maxLens) (\m' -> max m m')
                    mErr <- runExceptT $ evalExpressionM =<< renameExpressionM p
                    case mErr of
                        Right x  -> liftIO $ TIO.putStrLn x
                        Left err -> liftIO $ putDoc (pretty err)

-- TODO: check
loadFile :: FilePath -> Repl AlexPosn ()
loadFile fp = do
    contents <- liftIO $ BSL.readFile fp
    case parseWithCtx contents of
        Left err     -> liftIO $ putDoc (pretty err)
        Right (m, p) -> lift $ do
            -- TODO: think about global uniqueness
            modifying (rename.maxLens) (\m' -> max m m')
            loadDickinson =<< renameDickinsonM p
