module Test.HSpec.MarkdownFormatterSpec where

import Hydra.Prelude
import Test.Hspec.MarkdownFormatter
import Test.Hydra.Prelude

import Data.ByteString.Char8 (unpack)
import Data.List (isInfixOf)
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
import Test.QuickCheck (Positive (..), Small (..), counterexample, forAll, frequency, property, vectorOf)
import Test.QuickCheck.Monadic (assert, forAllM, monadic, monadicIO, monitor, pick, run)

spec :: Spec
spec =
  around (withTempDir "foo") $ do
    it "generates markdown from Spec laying out all labels hierarchically and property runs details" $ \tmpDir ->
      hspecWithMarkdown "Formatter Test" tmpDir testSpec
        `shouldReturn` "# Formatter Test\
                       \\n\n\
                       \## Some Spec\
                       \\n\n\
                       \### Sub Spec\
                       \\n\n\
                       \* does one thing\n\
                       \* does two things\n\
                       \* check a property\n\
                       \  <details>\n\
                       \  <summary>Details</summary>\n\
                       \  \n\
                       \  ```\n\
                       \  +++ OK, passed 100 tests.\n\
                       \  ```\n\
                       \  \n\
                       \  </details>"

    it "generates markdown content to file when running spec" $ \tmpDir ->
      property $
        monadicIO $ do
          aSpecTree <- pick (genDescribe 3)
          content <- run $ hspecWithMarkdown "Test" tmpDir (toSpec aSpecTree)
          monitor (counterexample content)
          assert $ all (`isInfixOf` content) $ listLabels aSpecTree

hspecWithMarkdown :: String -> FilePath -> Spec -> IO String
hspecWithMarkdown title tmpDir aSpec = do
  let markdownFile = tmpDir </> "result.md"
  _summary <-
    hspecWithResult
      defaultConfig
        { configIgnoreConfigFile = True -- Needed to ensure we don't mess up this run with our default config
        , configFormat = Just (markdownFormatter title markdownFile)
        }
      aSpec
  unpack <$> readFileBS markdownFile

listLabels :: TestTree -> [String]
listLabels = go []
 where
  go acc (Describe s tts) = foldMap (go (s : acc)) tts
  go acc (It s) = s : acc

toSpec :: TestTree -> Spec
toSpec = \case
  Describe s tts ->
    describe s $ forM_ tts toSpec
  It s ->
    it s $ True `shouldBe` True

data TestTree
  = Describe String [TestTree]
  | It String
  deriving (Eq, Show)

genDescribe :: Int -> Gen TestTree
genDescribe maxDepth = do
  (Positive (Small n)) <- arbitrary
  Describe <$> someLabel <*> vectorOf n (genSpecTree (maxDepth - 1))

genSpecTree :: Int -> Gen TestTree
genSpecTree 0 = It <$> someLabel
genSpecTree maxDepth =
  frequency
    [ (5, It <$> someLabel)
    ,
      ( 1
      , genDescribe maxDepth
      )
    ]

someLabel :: Gen String
someLabel = do
  Positive (n :: Int) <- arbitrary
  pure $ "test-" <> show n

testSpec :: Spec
testSpec =
  describe "Some Spec" $
    describe "Sub Spec" $ do
      it "does one thing" $ True `shouldBe` True
      it "does two things" $ True `shouldBe` True
      prop "check a property" $ \(n :: Int) -> n == n `div` 1
