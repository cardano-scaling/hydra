module Test.HSpec.MarkdownFormatterSpec where

import Data.List (isInfixOf)
import Hydra.Prelude
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName, (</>))
import Test.Hspec.Core.Format (Event (..), Format, FormatConfig)
import Test.Hspec.Core.Runner (
  Config (..),
  Summary (..),
  defaultConfig,
  hspecWith,
  hspecWithResult,
 )
import Test.Hspec.MarkdownFormatter
import Test.Hydra.Prelude
import Test.QuickCheck (forAll, property)
import Test.QuickCheck.Monadic (assert, forAllM, monadic, monadicIO, run)

testSpec :: Spec
testSpec =
  describe "Some Spec" $
    describe "Sub Spec" $ do
      it "does one thing" $ True `shouldBe` True
      it "does two things" $ True `shouldBe` True

spec :: Spec
spec =
  around (withTempDir "foo") $
    it "generates markdown content to given file when running spec" $ \tmpDir ->
      property $
        monadicIO $
          forAllM genSpecTree $ \aSpecTree -> do
            content <- run $ do
              let markdownFile = tmpDir </> "result.md"
              summary <-
                hspecWithResult
                  defaultConfig
                    { configIgnoreConfigFile = True -- Needed to ensure we don't mess up this run with our default config
                    , configFormat = Just (markdownFormatter markdownFile)
                    }
                  (toSpec aSpecTree)
              readFile markdownFile
            assert $ "foo" `isInfixOf` content

toSpec :: TestTree -> Spec
toSpec = error "not implemented"

data TestTree
  = Describe String [TestTree]
  | It String
  deriving (Eq, Show)

genSpecTree :: Gen TestTree
genSpecTree = error "not implemented"
