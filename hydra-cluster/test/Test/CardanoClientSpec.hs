module Test.CardanoClientSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "aeson" Data.Aeson ((.:))
import "aeson" Data.Aeson qualified as Aeson
import "filepath" System.FilePath ((</>))
import "hydra-cardano-api" Hydra.Cardano.Api (GenesisParameters (..))
import "hydra-node" Hydra.Chain.Backend qualified as Backend
import "hydra-node" Hydra.Logging (showLogsOnFailure)
import "hydra-node" Hydra.Utils (readJsonFileThrow)

import Test.EndToEndSpec (withClusterTempDir)
import CardanoNode (withCardanoNodeDevnet)

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
