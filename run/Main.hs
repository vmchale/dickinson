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
import           System.Directory         (doesFileExist)
import           System.Environment       (lookupEnv)
import           System.FilePath          ((</>))

-- TODO debug/verbosity options...
data Act = Run !FilePath ![FilePath]
         | REPL ![FilePath]
         | Check ![FilePath] ![FilePath]
         | Lint ![FilePath] ![FilePath]
         | Format !FilePath Bool
         | Man
         | Ide !FilePath ![FilePath]

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
    )
    <|> hsubparser (command "ide" (info ide (progDesc "Run all checks and lints")) <> internal)
    <|> runP

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
lintP = Lint <$> some dckFile <*> includes

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

manFind :: IO FilePath
manFind = do
    cabalData <- (</> "emd.1") . (</> "man") <$> getDataDir
    cabalInstall <- doesFileExist cabalData
    if cabalInstall
        then pure cabalData
        else do
            mHome <- lookupEnv "HOME"
            case mHome of
                Just h  -> pure $ h </> ".local" </> "share" </> "man" </> "man1" </> "emd.1"
                Nothing -> error "Could not determine home directory! I don't know where your manpages are installed!"

run :: Act -> IO ()
run (Run fp is)       = do { is' <- modIs is ; TIO.putStrLn =<< pipeline is' fp }
run (REPL fps)        = dickinsonRepl fps
run (Check fs i)      = do { is' <- modIs i ; traverse_ (validateFile is') fs }
run (Lint fs is)      = do { is' <- modIs is ; traverse_ warnFile fs ; traverse_ (patternExhaustivenessFile is') fs }
run (Format fp False) = fmtFile fp
run (Format fp True)  = fmtInplace fp
run Man               = putStrLn =<< manFind
run (Ide fp is)       = do { is' <- modIs is ; validateFile is' fp ; warnFile fp ; patternExhaustivenessFile is' fp }
