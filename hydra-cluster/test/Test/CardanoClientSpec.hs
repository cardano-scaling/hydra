module Test.CardanoClientSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (QueryPoint (..), queryCurrentEra, queryGenesisParameters)
import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Hydra.Cardano.Api (AnyCardanoEra (..), GenesisParameters (..))
import Hydra.Ledger.Cardano.Configuration (readJsonFileThrow)
import Hydra.Logging (showLogsOnFailure)
import System.FilePath ((</>))
import Test.EndToEndSpec (withClusterTempDir)

spec :: Spec
spec =
  around (showLogsOnFailure "CardanoClientSpec") $
    it "queryGenesisParameters works as expected" $ \tracer ->
      failAfter 60 $
        withClusterTempDir "queryGenesisParameters" $ \tmpDir -> do
          -- This uses the hydra-cluster/config/devnet and updates the
          -- systemStart to some current time making it the perfect target to
          -- test against.
          withCardanoNodeDevnet tracer tmpDir $ \RunningNode{nodeSocket, networkId} -> do
            AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
            GenesisParameters{protocolParamSystemStart = queriedSystemStart} <-
              queryGenesisParameters networkId nodeSocket QueryTip era

            let parseSystemStart =
                  Aeson.withObject "GenesisShelley" $ \o -> o .: "systemStart"
            expectedSystemStart <- readJsonFileThrow parseSystemStart $ tmpDir </> "genesis-shelley.json"

            queriedSystemStart `shouldBe` expectedSystemStart
