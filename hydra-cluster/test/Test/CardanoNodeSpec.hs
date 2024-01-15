module Test.CardanoNodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (
  getCardanoNodeVersion,
  withCardanoNodeDevnet,
  withCardanoNodeOnKnownNetwork,
 )

import CardanoClient (RunningNode (..), queryTipSlotNo)
import Hydra.Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic), unFile)
import Hydra.Cardano.Api qualified as NetworkId
import Hydra.Cluster.Fixture (KnownNetwork (Mainnet))
import Hydra.Logging (showLogsOnFailure)
import System.Directory (doesFileExist)

spec :: Spec
spec = do
  -- NOTE: We also hard-code the cardano-node version here to allow prevent
  -- false positives test errors in case someone uses an "untested" /
  -- different than in shell.nix version of cardano-node and cardano-cli.
  it "has expected cardano-node version available" $
    getCardanoNodeVersion >>= (`shouldContain` "8.7.2")

  it "withCardanoNodeDevnet does start a block-producing devnet within 5 seconds" $
    failAfter 5 $
      showLogsOnFailure "CardanoNodeSpec" $ \tr ->
        withTempDir "hydra-cluster" $ \tmp ->
          withCardanoNodeDevnet tr tmp $
            \RunningNode{nodeSocket, networkId, blockTime} -> do
              doesFileExist (unFile nodeSocket) `shouldReturn` True
              -- NOTE: We hard-code the expected networkId and blockTime here to
              -- detect any change to the genesis-shelley.json
              networkId `shouldBe` Testnet (NetworkMagic 42)
              blockTime `shouldBe` 0.1
              -- Should produce blocks (tip advances)
              slot1 <- queryTipSlotNo networkId nodeSocket
              threadDelay 1
              slot2 <- queryTipSlotNo networkId nodeSocket
              slot2 `shouldSatisfy` (> slot1)

  it "withCardanoNodeOnKnownNetwork on mainnet starts synchronizing within 5 seconds" $
    -- NOTE: This implies that withCardanoNodeOnKnownNetwork does not
    -- synchronize the whole chain before continuing.
    failAfter 5 $
      showLogsOnFailure "CardanoNodeSpec" $ \tr ->
        withTempDir "hydra-cluster" $ \tmp ->
          withCardanoNodeOnKnownNetwork tr tmp Mainnet $
            \RunningNode{nodeSocket, networkId, blockTime} -> do
              networkId `shouldBe` NetworkId.Mainnet
              blockTime `shouldBe` 20
              -- Should synchronize blocks (tip advances)
              slot1 <- queryTipSlotNo networkId nodeSocket
              threadDelay 1
              slot2 <- queryTipSlotNo networkId nodeSocket
              slot2 `shouldSatisfy` (> slot1)
