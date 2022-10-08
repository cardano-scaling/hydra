{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

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
      void $ hspecWithResult defaultConfig{configFormat = Just (markdownFormatter markdownFile)} testSpec
      content <- readFile markdownFile
      content `shouldContain` "# Some Spec"

markdownFormatter :: FilePath -> FormatConfig -> IO Format
markdownFormatter outputFile config = pure $ \case
  Done paths -> do
    time <- getCurrentTime

    let (directory, _) = splitFileName outputFile
    createDirectoryIfMissing True directory

    writeFile outputFile "# Some Spec"
  _ -> pure ()
