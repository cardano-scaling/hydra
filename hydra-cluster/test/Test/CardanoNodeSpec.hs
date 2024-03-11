module Test.CardanoNodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (
  findRunningCardanoNode,
  getCardanoNodeVersion,
  withCardanoNodeDevnet,
  withCardanoNodeOnKnownNetwork,
 )

import CardanoClient (RunningNode (..), queryTipSlotNo)
import Hydra.Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic), unFile)
import Hydra.Cluster.Fixture (KnownNetwork (..))
import Hydra.Logging (Tracer, showLogsOnFailure)
import System.Directory (doesFileExist)

spec :: Spec
spec = do
  -- NOTE: We also hard-code the cardano-node version here to allow prevent
  -- false positives test errors in case someone uses an "untested" /
  -- different than in shell.nix version of cardano-node and cardano-cli.
  it "has expected cardano-node version available" $
    getCardanoNodeVersion >>= (`shouldContain` "8.8.0")

  around (failAfter 5 . setupTracerAndTempDir) $ do
    it "withCardanoNodeDevnet does start a block-producing devnet within 5 seconds" $ \(tr, tmp) ->
      withCardanoNodeDevnet tr tmp $ \RunningNode{nodeSocket, networkId, blockTime} -> do
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

    it "withCardanoNodeOnKnownNetwork on mainnet starts synchronizing within 5 seconds" $ \_ ->
      pendingWith "cardano-node 8.8 is not supported on mainnet (config mismatch)"

    it "withCardanoNodeOnKnownNetwork on sanchonet starts synchronizing within 5 seconds" $ \(tr, tmp) ->
      -- NOTE: This implies that withCardanoNodeOnKnownNetwork does not
      -- synchronize the whole chain before continuing.
      withCardanoNodeOnKnownNetwork tr tmp Sanchonet $ \RunningNode{nodeSocket, networkId, blockTime} -> do
        networkId `shouldBe` Testnet (NetworkMagic 4)
        blockTime `shouldBe` 20
        -- Should synchronize blocks (tip advances)
        slot1 <- queryTipSlotNo networkId nodeSocket
        threadDelay 1
        slot2 <- queryTipSlotNo networkId nodeSocket
        slot2 `shouldSatisfy` (> slot1)

    describe "findRunningCardanoNode" $ do
      it "returns Nothing on non-matching network" $ \(tr, tmp) -> do
        withCardanoNodeOnKnownNetwork tr tmp Sanchonet $ \_ -> do
          findRunningCardanoNode tr tmp Preproduction `shouldReturn` Nothing

      it "returns Just running node on matching network" $ \(tr, tmp) -> do
        withCardanoNodeOnKnownNetwork tr tmp Sanchonet $ \runningNode -> do
          findRunningCardanoNode tr tmp Sanchonet `shouldReturn` Just runningNode

setupTracerAndTempDir :: ToJSON msg => ((Tracer IO msg, FilePath) -> IO a) -> IO a
setupTracerAndTempDir action =
  showLogsOnFailure "CardanoNodeSpec" $ \tr ->
    withTempDir "hydra-cluster" $ \tmp ->
      action (tr, tmp)
