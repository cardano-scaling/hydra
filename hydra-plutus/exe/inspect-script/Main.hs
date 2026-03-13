module Main where

import Hydra.Prelude

import Hydra.Contract (hydraScriptCatalogue)

main :: IO ()
main = do
  putTextLn "Hydra script catalogue:"
  putLBSLn $ encodePretty hydraScriptCatalogue
