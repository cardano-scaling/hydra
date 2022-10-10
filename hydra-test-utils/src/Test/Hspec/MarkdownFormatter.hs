module Test.Hspec.MarkdownFormatter where

import Hydra.Prelude hiding (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.Hspec.Core.Format (Event (..), Format, FormatConfig, Item, Path)

markdownFormatter :: FilePath -> FormatConfig -> IO Format
markdownFormatter outputFile _config = do
  createDirectoryIfMissing True (fst $ splitFileName outputFile)
  pure $ \case
    Done paths -> do
      let itemsTree = pathsToTree paths
      writeFile outputFile $ foldMap toMarkdown itemsTree
    _else -> pure ()

pathsToTree :: [(Path, Item)] -> [Tree]
pathsToTree =
  foldr (growForest 0) []

growForest :: Int -> (Path, Item) -> [Tree] -> [Tree]
growForest lvl (path, item) forest =
  case (path, forest) of
    ((root : rest, itemDesc), Group desc _ subs : groups)
      | root == desc ->
        let subs' = growForest (lvl + 1) ((rest, itemDesc), item) subs
         in Group desc (lvl + 1) subs' : groups
      | otherwise -> Group desc (lvl + 1) subs : growForest lvl (path, item) groups
    ((root : rest, itemDesc), []) ->
      [Group root (lvl + 1) (growForest (lvl + 1) ((rest, itemDesc), item) [])]
    (([], itemDesc), groups) ->
      Test itemDesc : groups
    other -> error $ "unhandled case " <> show other

type Description = String
type Level = Int

data Tree
  = Group Description Level [Tree]
  | Test Description
  deriving (Eq, Show)

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
toMarkdown Root = ""
