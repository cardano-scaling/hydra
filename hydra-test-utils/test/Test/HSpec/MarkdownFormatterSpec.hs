module Test.HSpec.MarkdownFormatterSpec where

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

testSpec :: Spec
testSpec =
  describe "Some Spec" $
    it "does one thing" $ True `shouldBe` True

spec :: Spec
spec =
  it "generates markdown content to given file when running spec" $
    withTempDir "mdformat" $ \tmpDir -> do
      let markdownFile = tmpDir </> "result.md"
      summary <-
        hspecWithResult
          defaultConfig
            { configIgnoreConfigFile = True -- Needed to ensure we don't mess up this run with our default config
            , configFormat = Just (markdownFormatter markdownFile)
            }
          testSpec
      content <- readFile markdownFile
      content `shouldContain` "# Some Spec"
      content `shouldContain` "* does one thing"
