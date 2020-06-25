module Golden ( goldenTests
              ) where

import           Control.Exception             (throw)
import qualified Data.ByteString.Lazy          as BSL
import           Data.Text.Lazy.Encoding       (encodeUtf8)
import           Data.Text.Prettyprint.Doc     (pretty)
import           Data.Text.Prettyprint.Doc.Ext
import           Language.Dickinson.Parser
import           Language.Dickinson.Rename
import           Language.Dickinson.Type
import           System.FilePath               ((-<.>))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Golden             (goldenVsString)

goldenTests :: TestTree
goldenTests =
    testGroup "Golden tests"
        [ withDckFile "test/data/nestLet.dck"
        , withDckFile "test/data/import.dck"
        , renameDckFile "test/data/nestLet.dck"
        , renameDckFile "test/data/let.dck"
        , renameDckFile "test/data/multiLet.dck"
        , renameDckFile "test/data/lambda.dck"
        , renameDckFile "test/data/higherOrder.dck"
        , renameDckFile "test/data/tupleRename.dck"
        ]

prettyBSL :: Dickinson a -> BSL.ByteString
prettyBSL = encodeUtf8 . dickinsonLazyText . pretty

withDckFile :: FilePath -> TestTree
withDckFile fp =
    goldenVsString ("Matches golden output " ++ fp) (fp -<.> "pretty") act

    where act = prettyBSL . yeet . parse <$> BSL.readFile fp
          yeet = either throw id

renameDckFile :: FilePath -> TestTree
renameDckFile fp =
    goldenVsString ("Matches golden output " ++ fp) (fp -<.> "rename") act

    where act = prettyBSL . fst . uncurry renameDickinson . yeet . parseWithMax <$> BSL.readFile fp
          yeet = either throw id
