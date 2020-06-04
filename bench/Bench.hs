module Main (main) where

import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Language.Dickinson   (lexDickinson, parse)

main :: IO ()
main =
    defaultMain [ env libFile $ \c ->
                  bgroup "lib/color.dck"
                    [ bench "parse" $ nf parse c
                    , bench "lex" $ nf lexDickinson c
                    ]
                ]

    where libFile = BSL.readFile "lib/color.dck"
