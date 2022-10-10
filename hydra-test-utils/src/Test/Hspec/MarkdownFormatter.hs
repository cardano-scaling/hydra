module Test.Hspec.MarkdownFormatter (markdownFormatter) where

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Hydra.Prelude hiding (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.Hspec.Core.Format (Event (..), Format, FormatConfig, Item, Path)

-- | Generates a markdowm-formatted test tree into a file.
--
-- Using the formatter requires to modify the configuration used by `hspec` to
-- run the tests.
--
-- @@
-- import qualified MySpec as Spec
--
-- main :: IO ()
-- main = hspecWith
--    defaultConfig{configFormat = Just (markdownFormatter "hspec-results.md")}
--    MySpec.spec
-- @@
--
-- FIXME: It seems the way to add formatters changed in 2.10 so perhaps this
-- might not work in all settings.
markdownFormatter :: FilePath -> FormatConfig -> IO Format
markdownFormatter outputFile _config = do
  createDirectoryIfMissing True (fst $ splitFileName outputFile)
  pure $ \case
    -- NOTE: one would be tempted to use the various `Event`s provided by hspec
    -- formatter API (see https://hackage.haskell.org/package/hspec-core-2.10.6/docs/Test-Hspec-Core-Format.html#t:Event)
    -- to "naturally" output a markdown layout isomorphic to the tree structure of Specs
    -- but it turns out that:
    --  1. These events are apparently not emitted by default prior to 2.10 (??!!)
    --  2. What if one runs tests in parallel? We would need to accumulate and track the various parts in some
    --     random order anyway
    --
    -- So this is annoying but using `Done` is the safest path, even though it implies
    -- reconstructing the tree of tests from the individual paths which is somewhat
    -- silly.
    Done paths -> do
      let markdown = foldMap toMarkdown $ pathsToTree paths
      BS.writeFile outputFile $ encodeUtf8 $ Text.strip $ Text.pack markdown
    _else -> pure ()

pathsToTree :: [(Path, Item)] -> [Tree]
pathsToTree =
  foldr (growForest 0) []
 where
  growForest :: Int -> (Path, Item) -> [Tree] -> [Tree]
  growForest lvl (path, item) forest =
    case (path, forest) of
      ((root : rest, itemDesc), Group desc _ subs : groups)
        | root == desc ->
          let subs' = growForest (lvl + 1) ((rest, itemDesc), item) subs
           in Group desc (lvl + 1) subs' : groups
        | otherwise ->
          Group desc (lvl + 1) subs : growForest lvl (path, item) groups
      ((root : rest, itemDesc), groups) ->
        let subs = growForest (lvl + 1) ((rest, itemDesc), item) []
         in Group root (lvl + 1) subs : groups
      (([], itemDesc), groups) ->
        Test itemDesc : groups

type Description = String
type Level = Int

data Tree
  = Group Description Level [Tree]
  | Test Description

-- TODO: this function uses naive string concatenation, using Doc from any
-- prettyprinter package would provide better layout
toMarkdown :: Tree -> String
toMarkdown (Group description level subTrees) =
  header
    <> description
    <> "\n\n"
    <> concatMap toMarkdown subTrees
    <> "\n"
 where
  header = replicate level '#' <> " "
toMarkdown (Test description) = "* " <> description <> "\n"
