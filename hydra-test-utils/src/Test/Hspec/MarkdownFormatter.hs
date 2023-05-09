module Test.Hspec.MarkdownFormatter (markdownFormatter) where

import Hydra.Prelude hiding (intercalate)

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.Hspec.Core.Format (
  Event (..),
  Format,
  FormatConfig,
  Item (..),
  Path,
 )

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
-- The resulting markdown content follows these conventions:
--
--  * The `title` is written as a level 1 header at the top of the file,
--  * Every `describe` statement generates a new header one level below the enclosing one,
--  * Every `it` or `specify` statement yields a list item,
--  * Additional info output by a test (eg. QuickCheck's property count) will be added as
--    pre-formatted text inside a foldable _Details_ section under its item.
--
-- NOTE: It seems the way to add formatters changed in 2.10 so perhaps this
-- might not work in all settings.
markdownFormatter :: String -> FilePath -> FormatConfig -> IO Format
markdownFormatter title outputFile _config = do
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
      let markdown = "# " <> title <> "\n\n" <> foldMap toMarkdown (pathsToTree 1 paths)
      BS.writeFile outputFile $ encodeUtf8 $ Text.strip $ Text.pack markdown
    _else -> pure ()

pathsToTree :: Level -> [(Path, Item)] -> [Tree]
pathsToTree startLevel =
  foldr (growForest startLevel) []
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
        Test itemDesc item : groups

type Description = String
type Level = Int

data Tree
  = Group Description Level [Tree]
  | Test Description Item

toMarkdown :: Tree -> String
toMarkdown (Group description level subTrees) =
  header
    <> description
    <> "\n\n"
    <> concatMap toMarkdown subTrees
    <> "\n"
 where
  header = replicate level '#' <> " "
toMarkdown (Test description Item{itemInfo}) =
  "* " <> description <> "\n" <> moreInfo
 where
  moreInfo =
    if null itemInfo
      then ""
      else
        List.unlines $
          ["  <details>", "  <summary>Details</summary>", "  ", "  ```"]
            <> map ("  " <>) (List.lines itemInfo)
            <> ["  ```", "  ", "  </details>"]
