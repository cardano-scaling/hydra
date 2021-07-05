module Main where

import Hydra.Prelude
import qualified Spec
import Test.HSpec.JUnit
import Test.Hspec.Core.Format (Format, FormatConfig)
import Test.Hspec.Core.Formatters (formatterToFormat, specdoc)
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig{configFormat = Just dualFormatter} Spec.spec

dualFormatter :: FormatConfig -> IO Format
dualFormatter config = do
  junit <- junitFormat "test-results.xml" "hydra-node" config
  docSpec <- formatterToFormat specdoc config
  pure $ \e -> junit e >> docSpec e
