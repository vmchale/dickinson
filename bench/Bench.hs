module Main (main) where

import           Control.Exception    (throw)
import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Language.Dickinson

main :: IO ()
main =
    defaultMain [ env libFile $ \c ->
                  bgroup "lib/color.dck"
                    [ bench "parse" $ nf parse c
                    , bench "lex" $ nf lexDickinson c
                    ]
                , env libParsed $ \p ->
                  bgroup "renamer"
                    [ bench "bench/data/nestLet.dck" $ nf plainExpr p
                    ]
                ]

    where libFile = BSL.readFile "lib/color.dck"
          libParsed = either throw id . parseWithCtx <$> BSL.readFile "bench/data/nestLet.dck"

plainExpr :: (UniqueCtx, Dickinson Name a) -> Dickinson Name a
plainExpr = fst . uncurry renameDickinson
