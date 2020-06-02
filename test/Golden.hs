module Golden ( goldenTests
              ) where

import qualified Data.ByteString.Lazy    as BSL
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Language.Dickinson
import           System.FilePath         ((-<.>))
import           Test.Tasty
import           Test.Tasty.Golden

goldenTests = withDckFile "test/data/nestLet.dck"

prettyBSL :: Pretty (name a) => Dickinson name a -> BSL.ByteString
prettyBSL = encodeUtf8 . prettyLazyText

withDckFile :: FilePath -> TestTree
withDckFile fp =
    goldenVsString ("Matches golden output " ++ fp) (fp -<.> "out") act

    where act = prettyBSL . yeet . parse <$> BSL.readFile fp
          yeet = either (error.show) id
