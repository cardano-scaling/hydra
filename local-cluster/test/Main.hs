module Main where

import Hydra.Prelude
import qualified Spec
import Test.HSpec.JUnit
import Test.Hspec.Core.Format (Format, FormatConfig)
import Test.Hspec.Core.Formatters (formatterToFormat, specdoc)
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig{configFormat = Just dualFormatter} Spec.spec

-- NOTE: This is duplicated from hydra-node's tests, we should provide this as part
-- of a standard 'Hydra.Test.Prelude' but should this live in hydra-prelude or in
-- a new package?
dualFormatter :: FormatConfig -> IO Format
dualFormatter config = do
  junit <- junitFormat "test-results.xml" "local-cluster" config
  docSpec <- formatterToFormat specdoc config
  pure $ \e -> junit e >> docSpec e
