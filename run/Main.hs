module Main (main) where

import           Data.Foldable            (traverse_)
import           Data.Semigroup
import qualified Data.Text.IO             as TIO
import           Format
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
         | Check ![FilePath] ![FilePath]
         | Lint ![FilePath]
         | Format !FilePath Bool
         | Man
         | Ide !FilePath ![FilePath]
         | Dir

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
    <> command "ide" (info ide (progDesc "Run all checks and lints"))
    -- ide and dir should be hidden?
    <> command "dir" (info (pure Dir) (progDesc "Show library install directory"))
    ) <|> runP

formatP :: Parser Act
formatP = Format
    <$> dckFile
    <*> switch (long "inplace" <> short 'i' <> help "Overwrite file in-place")

replP :: Parser Act
replP = REPL <$> many dckFile

runP :: Parser Act
runP = Run <$> dckFile <*> includes

ide :: Parser Act
ide = Ide <$> dckFile <*> includes

checkP :: Parser Act
checkP = Check <$> some dckFile <*> includes

lintP :: Parser Act
lintP = Lint <$> some dckFile

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

modIs :: [FilePath] -> IO [FilePath]
modIs is =
    defaultLibPath <*> ((++) <$> dckPath <*> pure is)

run :: Act -> IO ()
run (Run fp is)       = do { is' <- modIs is ; TIO.putStrLn =<< pipeline is' fp }
run (REPL fps)        = dickinsonRepl fps
run (Check fs i)      = do { is' <- modIs i ; traverse_ (validateFile is') fs }
run (Lint fs)         = traverse_ warnFile fs
run (Format fp False) = fmtFile fp
run (Format fp True)  = fmtInplace fp
run Man               = putStrLn . (</> "emd.1") . (</> "man") =<< getDataDir
run (Ide fp is)       = do { is' <- modIs is ; validateFile is' fp ; warnFile fp }
run Dir               = putStrLn =<< getDataDir
