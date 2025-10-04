module Test.CardanoClientSpec where

import Hydra.Prelude
import Test.Hspec (Spec, around, it, shouldBe)
import Test.Hydra.Prelude

import CardanoNode (withCardanoNodeDevnet)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Hydra.Cardano.Api (GenesisParameters (..))
import Hydra.Chain.Backend qualified as Backend
import Hydra.Logging (showLogsOnFailure)
import Hydra.Utils (readJsonFileThrow)
import System.FilePath ((</>))
import Test.EndToEndSpec (withClusterTempDir)

spec :: Spec
spec =
  around (showLogsOnFailure "CardanoClientSpec") $
    it "queryGenesisParameters works as expected" $ \tracer ->
      failAfter 60 $
        withClusterTempDir $ \tmpDir -> do
          -- This uses the hydra-cluster/config/devnet and updates the
          -- systemStart to some current time making it the perfect target to
          -- test against.
          withCardanoNodeDevnet tracer tmpDir $ \_ backend -> do
            GenesisParameters{protocolParamSystemStart = queriedSystemStart} <-
              Backend.queryGenesisParameters backend

            let parseSystemStart =
                  Aeson.withObject "GenesisShelley" $ \o -> o .: "systemStart"
            expectedSystemStart <- readJsonFileThrow parseSystemStart $ tmpDir </> "genesis-shelley.json"

            queriedSystemStart `shouldBe` expectedSystemStart
