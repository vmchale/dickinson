module REPL ( dickinsonRepl
            ) where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (StateT)
import qualified Data.ByteString.Lazy     as BSL
import           Language.Dickinson
import           System.Console.Haskeline (InputT)

dickinsonRepl :: IO ()
dickinsonRepl = putStrLn "Not yet implemented."

-- repl lexer?
-- "emd> "

type Repl a = InputT (StateT (EvalSt a) IO)


-- eval expression OR load file
-- also should be able to set stuff

loadFile :: FilePath -> Repl AlexPosn ()
loadFile fp = do
    contents <- liftIO $ BSL.readFile fp
    pure ()
