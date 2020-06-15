module REPL ( dickinsonRepl
            ) where

import           Language.Dickinson
import           System.Console.Haskeline (InputT)

dickinsonRepl :: IO ()
dickinsonRepl = putStrLn "Not yet implemented."

-- repl lexer?
-- "emd> "

type REPL a = InputT (EvalT IO a)
