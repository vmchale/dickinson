{-# LANGUAGE TemplateHaskell #-}

-- | @since 0.1.2.0
module Language.Dickinson.QuasiQuoter ( dickinson
                                      , run
                                      ) where

import           Control.Exception.Value    (eitherThrow)
import           Data.Data                  (Data)
import           Data.Foldable              (traverse_)
import qualified Data.Text                  as T
import           Data.Typeable              (cast)
import           Language.Dickinson.Eval
import           Language.Dickinson.File
import           Language.Dickinson.Lexer
import           Language.Dickinson.Type
import           Language.Haskell.TH        (Exp, Q)
import           Language.Haskell.TH.Syntax (Exp (AppE, VarE))
import qualified Language.Haskell.TH.Syntax as TH

run :: [Declaration AlexPosn] -> IO T.Text
run = fmap eitherThrow . evalIO . evalDickinsonAsMain

dickinson :: [FilePath] -> FilePath -> Q Exp
dickinson is fp = do
    traverse_ TH.addDependentFile (fp:is) -- TODO: resolve dependencies
    ds <- TH.runIO (validateAmalgamate is fp)
    liftDataWithText ds

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> TH.lift (T.unpack txt)

-- see this mess: https://stackoverflow.com/a/38182444
liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = TH.dataToExpQ (fmap liftText . cast)
