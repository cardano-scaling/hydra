-- | Render escaped labels as a linear Mermaid diagram. No protocol dependency.
module Test.Hydra.HeadLogic.Mermaid (renderLinearTrace) where

import Data.Text qualified as Text
import Hydra.Prelude

renderLinearTrace :: [Text] -> Text
renderLinearTrace steps =
  Text.unlines $
    ["flowchart TD"]
      <> zipWith node [0 :: Int ..] steps
      <> zipWith edge [0 :: Int ..] (drop 1 steps)
 where
  node :: Int -> Text -> Text
  node n step = "  n" <> show n <> "[\"" <> escape step <> "\"]"
  edge :: Int -> Text -> Text
  edge n _ = "  n" <> show n <> " --> n" <> show (n + 1)
  escape :: Text -> Text
  escape = Text.replace "\n" " " . Text.replace ">" "&gt;" . Text.replace "<" "&lt;" . Text.replace "\"" "&quot;" . Text.replace "&" "&amp;"
