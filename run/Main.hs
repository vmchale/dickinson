module Main (main) where

import Options.Applicative
import Language.Dickinson
import qualified Data.Text.IO as TIO

-- TODO debug/verbosity options...
data Act = Run !FilePath

main :: IO ()
main = run =<< execParser wrapper

act :: Parser Act
act = hsubparser
    (command "run" (info runP (progDesc "Execute a file")))

runP :: Parser Act
runP = Run
    <$> argument str
    (metavar "FILE"
    <> help "Source file"
    <> dckCompletions)

dckCompletions :: HasCompleter f => Mod f a
dckCompletions = completer . bashCompleter $ "file -X '*.dck' -o plusdirs"

wrapper :: ParserInfo Act
wrapper = info (helper <*> versionMod <*> act)
    (fullDesc
    <> progDesc "Dickinson text-generation language"
    <> header "Dickinson - a text-generation language")

versionMod :: Parser (a -> a)
versionMod = infoOption languageDickinsonVersionString (short 'V' <> long "version" <> help "Show version")

run :: Act -> IO ()
run (Run fp) = TIO.putStrLn =<< evalFile fp
