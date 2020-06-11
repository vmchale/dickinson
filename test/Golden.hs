module Golden ( goldenTests
              ) where

import qualified Data.ByteString.Lazy          as BSL
import           Data.Text.Lazy.Encoding       (encodeUtf8)
import           Data.Text.Prettyprint.Doc.Ext
import           Language.Dickinson
import           System.FilePath               ((-<.>))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Golden             (goldenVsString)

goldenTests :: TestTree
goldenTests =
    testGroup "Golden tests"
        [ withDckFile "test/data/nestLet.dck"
        , renameDckFile "test/data/nestLet.dck"
        , renameDckFile "test/data/let.dck"
        ]

prettyBSL :: Pretty (name a) => Dickinson name a -> BSL.ByteString
prettyBSL = encodeUtf8 . dickinsonLazyText . prettyDickinson

withDckFile :: FilePath -> TestTree
withDckFile fp =
    goldenVsString ("Matches golden output " ++ fp) (fp -<.> "pretty") act

    where act = prettyBSL . yeet . parse <$> BSL.readFile fp
          yeet = either (error.show) id

renameDckFile :: FilePath -> TestTree
renameDckFile fp =
    goldenVsString ("Matches golden output " ++ fp) (fp -<.> "rename") act

    where act = prettyBSL . fst . uncurry renameDickinson . yeet . parseWithCtx <$> BSL.readFile fp
          yeet = either (error.show) id
