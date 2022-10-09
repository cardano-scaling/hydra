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
      writeFile outputFile $ toMarkdown itemsTree
    _else -> pure ()

pathsToTree :: [(Path, Item)] -> Tree
pathsToTree =
  foldr populateTree Root
 where
  mkNode :: Description -> (Tree, Level) -> (Tree, Level)
  mkNode desc (t, lvl) = (Group desc (lvl - 1) [t], lvl - 1)

  populateTree :: (Path, Item) -> Tree -> Tree
  populateTree ((path, desc), _) _ = fst $ foldr mkNode (Test desc, length path + 1) path

type Description = String
type Level = Int

data Tree
  = Group Description Level [Tree]
  | Test Description
  | Root

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
