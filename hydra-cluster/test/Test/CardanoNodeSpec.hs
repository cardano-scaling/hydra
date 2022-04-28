module Test.CardanoNodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

-- Unit under test
import CardanoNode

import Hydra.Logging (showLogsOnFailure)
import System.Directory (doesFileExist)
import Test.Network.Ports (randomUnusedTCPPort)

spec :: Spec
spec = do
  -- NOTE: We also hard-code the cardano-node version here to allow prevent
  -- false positives test errors in case someone uses an "untested" /
  -- different than in shell.nix version of cardano-node and cardano-cli.
  it "has expected cardano-node version available" $
    getCardanoNodeVersion >>= (`shouldContain` "1.34.1")

  it "withBFTNode does start a node within 3 seconds" $
    failAfter 3 $
      showLogsOnFailure $ \tr -> do
        withTempDir "hydra-cluster" $ \tmp -> do
          systemStart <- initSystemStart
          ourPort <- randomUnusedTCPPort
          let config =
                CardanoNodeConfig
                  { nodeId = 1
                  , stateDirectory = tmp
                  , systemStart
                  , ports = PortsConfig{ours = ourPort, peers = []}
                  }
          withBFTNode tr config $ \(RunningNode _ socketFile) -> do
            doesFileExist socketFile `shouldReturn` True
