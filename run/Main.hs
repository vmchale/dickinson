module Main (main) where

import           Data.Semigroup
import qualified Data.Text.IO             as TIO
import           Language.Dickinson       (dickinsonVersionString)
import           Language.Dickinson.File
import           Language.Dickinson.Lib
import           Options.Applicative
import           Paths_language_dickinson (getDataDir)
import           REPL
import           System.FilePath          ((</>))

-- TODO debug/verbosity options...
data Act = Run !FilePath ![FilePath]
         | REPL ![FilePath]
         | Check !FilePath ![FilePath]
         | Lint !FilePath
         | Format !FilePath
         | Man

main :: IO ()
main = run =<< execParser wrapper

act :: Parser Act
act = hsubparser
    (command "run" (info runP (progDesc "Execute a file"))
    <> command "repl" (info replP (progDesc "Start a REPL"))
    <> command "check" (info checkP (progDesc "Check that some code is valid."))
    <> command "lint" (info lintP (progDesc "Examine a file for common errors."))
    <> command "fmt" (info formatP (progDesc "Format Dickinson code"))
    <> command "man" (info (pure Man) (progDesc "Dump path to manpages"))
    ) <|> runP

formatP :: Parser Act
formatP = Format <$> dckFile

replP :: Parser Act
replP = REPL <$> many dckFile

runP :: Parser Act
runP = Run <$> dckFile <*> includes

checkP :: Parser Act
checkP = Check <$> dckFile <*> includes

lintP :: Parser Act
lintP = Lint <$> dckFile

dckFile :: Parser FilePath
dckFile = argument str
    (metavar "FILE"
    <> help "Source file"
    <> dckCompletions)

includes :: Parser [FilePath]
includes = many $ strOption
    (metavar "DIR"
    <> long "include"
    <> short 'I'
    <> dirCompletions)

dckCompletions :: HasCompleter f => Mod f a
dckCompletions = completer . bashCompleter $ "file -X '!*.dck' -o plusdirs"

dirCompletions :: HasCompleter f => Mod f a
dirCompletions = completer . bashCompleter $ "directory"

wrapper :: ParserInfo Act
wrapper = info (helper <*> versionMod <*> act)
    (fullDesc
    <> progDesc "Dickinson text-generation language. See also 'man emd'"
    <> header "Dickinson - a text-generation language")

versionMod :: Parser (a -> a)
versionMod = infoOption dickinsonVersionString (short 'V' <> long "version" <> help "Show version")

run :: Act -> IO ()
run (Run fp is) = do { pGo <- defaultLibPath ; TIO.putStrLn =<< pipeline (pGo is) fp }
run (REPL _)    = dickinsonRepl
run (Check f i) = do { pathMod <- defaultLibPath ; validateFile (pathMod i) f } -- FIXME: reuse
run (Lint f)    = warnFile f
run (Format fp) = fmtFile fp
run Man         = putStrLn . (</> "emd.1") . (</> "man") =<< getDataDir
