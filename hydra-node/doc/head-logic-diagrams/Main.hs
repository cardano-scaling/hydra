module Main where

import Data.Text.IO qualified as Text
import Hydra.HeadLogic.Diagrams (page)
import Hydra.Prelude
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  content <- either (die . toString) pure page
  getArgs >>= \case
    [destination] -> createDirectoryIfMissing True (takeDirectory destination) >> Text.writeFile destination content
    _ -> die "Usage: head-logic-diagrams OUTPUT.md"
