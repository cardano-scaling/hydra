module Main where

import Hydra.HeadLogic.Diagrams (page)
import Hydra.Prelude
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  content <- either (die . toString) pure page
  getArgs >>= \case
    [destination] -> createDirectoryIfMissing True (takeDirectory destination) >> writeFileBS destination (encodeUtf8 content)
    _ -> die "Usage: head-logic-diagrams OUTPUT.md"
