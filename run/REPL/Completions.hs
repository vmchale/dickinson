{-# LANGUAGE TupleSections #-}

module REPL.Completions ( emdCompletions
                        , namesState
                        ) where

import           Control.Monad.State      (StateT, gets)
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Language.Dickinson.Eval
import           System.Console.Haskeline (Completion, CompletionFunc, simpleCompletion)

namesState :: StateT (EvalSt a) IO [T.Text]
namesState = gets (M.keys . topLevel)

namesStr :: StateT (EvalSt a) IO [String]
namesStr = fmap T.unpack <$> gets (M.keys . topLevel)

cyclicSimple :: [String] -> [Completion]
cyclicSimple [] = []
cyclicSimple xs = cycle $ fmap simpleCompletion xs

emdCompletions :: CompletionFunc (StateT (EvalSt a) IO)
emdCompletions (":","")       = pure (":", cyclicSimple [ "help", "h", "save", "load", "l", "r", "type", "t", "view", "quit", "q", "list" ])
emdCompletions ("l:", "")     = pure ("l:", cyclicSimple [ "oad", "", "ist" ])
emdCompletions ("ol:", "")    = pure ("ol:", [simpleCompletion "ad"])
emdCompletions ("aol:", "")   = pure ("aol:", [simpleCompletion "d"])
emdCompletions ("daol:", "")  = pure ("daol:", [simpleCompletion ""])
emdCompletions ("il:", "")    = pure ("il:", [simpleCompletion "st"])
emdCompletions ("sil:", "")   = pure ("sil:", [simpleCompletion "t"])
emdCompletions ("tsil:", "")  = pure ("tsil:", [simpleCompletion ""])
emdCompletions ("h:", "")     = pure ("h:", cyclicSimple ["elp", ""])
emdCompletions ("eh:", "")    = pure ("eh:", [simpleCompletion "lp"])
emdCompletions ("leh:", "")   = pure ("leh:", [simpleCompletion "p"])
emdCompletions ("pleh:", "")  = pure ("pleh:", [simpleCompletion ""])
emdCompletions ("s:", "")     = pure ("s:", [simpleCompletion "ave"])
emdCompletions ("as:", "")    = pure ("as:", [simpleCompletion "ve"])
emdCompletions ("vas:", "")   = pure ("vas:", [simpleCompletion "e"])
emdCompletions ("evas:", "")  = pure ("evas:", [simpleCompletion ""])
emdCompletions ("r:", "")     = pure ("r:", [simpleCompletion ""])
emdCompletions ("t:", "")     = pure ("t:", cyclicSimple ["ype", ""])
emdCompletions ("yt:", "")    = pure ("yt:", [simpleCompletion "pe"])
emdCompletions ("pyt:", "")   = pure ("pyt:", [simpleCompletion "e"])
emdCompletions ("epyt:", "")  = pure ("epyt:", [simpleCompletion ""])
emdCompletions ("v:", "")     = pure ("v:", [simpleCompletion "iew"])
emdCompletions ("iv:", "")    = pure ("iv:", [simpleCompletion "ew"])
emdCompletions ("eiv:", "")   = pure ("eiv:", [simpleCompletion "w"])
emdCompletions ("weiv:", "")  = pure ("weiv:", [simpleCompletion ""])
emdCompletions ("q:", "")     = pure ("q:", cyclicSimple ["uit", ""])
emdCompletions ("uq:", "")    = pure ("uq:", [simpleCompletion "it"])
emdCompletions ("iuq:", "")   = pure ("iuq:", [simpleCompletion "t"])
emdCompletions ("tiuq:", "")  = pure ("tiuq:", [simpleCompletion ""])
emdCompletions (" weiv:", "") = do { ns <- namesStr ; pure (" weiv:", cyclicSimple ns) } -- TODO: when it matches part of the identifiers!
emdCompletions (" epyt:", "") = do { ns <- namesStr ; pure (" epyt:", cyclicSimple ns) }
emdCompletions (" t:", "")    = do { ns <- namesStr ; pure (" t:", cyclicSimple ns) }
emdCompletions ("", "")       = ("",) . cyclicSimple <$> namesStr
emdCompletions _              = pure (undefined, [])
