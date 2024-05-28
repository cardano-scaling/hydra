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
import Hydra.Cluster.Fixture (KnownNetwork (..), toNetworkId)
import Hydra.Logging (Tracer, showLogsOnFailure)
import System.Directory (doesFileExist)
import Test.Hydra.Cluster.Utils (forEachKnownNetwork)

supportedNetworks :: [KnownNetwork]
supportedNetworks = [Sanchonet]

supportedCardanoNodeVersion :: String
supportedCardanoNodeVersion = "8.11.0"

forSupportedKnownNetworks :: String -> (KnownNetwork -> IO ()) -> Spec
forSupportedKnownNetworks msg action = forEachKnownNetwork msg $ \network -> do
  unless (network `elem` supportedNetworks) $
    pendingWith $
      "cardano-node " <> supportedCardanoNodeVersion <> " is only supported on " ++ show supportedNetworks
  action network

spec :: Spec
spec = do
  -- NOTE: We also hard-code the cardano-node version here to allow prevent
  -- false positives test errors in case someone uses an "untested" /
  -- different than in shell.nix version of cardano-node and cardano-cli.
  it "has expected cardano-node version available" $
    getCardanoNodeVersion >>= (`shouldContain` supportedCardanoNodeVersion)

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

    describe "findRunningCardanoNode" $ do
      it "returns Nothing on non-matching network" $ \(tr, tmp) -> do
        unless (Preview `elem` supportedNetworks) $
          pendingWith "Preview is not supported so skipping this test."
        withCardanoNodeOnKnownNetwork tr tmp Preview $ \_ -> do
          findRunningCardanoNode tr tmp Preproduction `shouldReturn` Nothing

      it "returns Just running node on matching network" $ \(tr, tmp) -> do
        unless (Preview `elem` supportedNetworks) $
          pendingWith "Preview is not supported so skipping this test."
        withCardanoNodeOnKnownNetwork tr tmp Preview $ \runningNode -> do
          findRunningCardanoNode tr tmp Preview `shouldReturn` Just runningNode

  forSupportedKnownNetworks "withCardanoNodeOnKnownNetwork starts synchronizing within 10 seconds" $ \network -> do
    -- NOTE: This implies that withCardanoNodeOnKnownNetwork does not
    -- synchronize the whole chain before continuing.
    setupTracerAndTempDir $ \(tr, tmp) ->
      withCardanoNodeOnKnownNetwork tr tmp network $ \RunningNode{nodeSocket, networkId, blockTime} -> do
        networkId `shouldBe` toNetworkId network
        blockTime `shouldBe` 20
        -- Should synchronize blocks (tip advances)
        slot1 <- queryTipSlotNo networkId nodeSocket
        threadDelay 10
        slot2 <- queryTipSlotNo networkId nodeSocket
        slot2 `shouldSatisfy` (> slot1)

setupTracerAndTempDir :: ToJSON msg => ((Tracer IO msg, FilePath) -> IO a) -> IO a
setupTracerAndTempDir action =
  showLogsOnFailure "CardanoNodeSpec" $ \tr ->
    withTempDir "hydra-cluster" $ \tmp ->
      action (tr, tmp)
