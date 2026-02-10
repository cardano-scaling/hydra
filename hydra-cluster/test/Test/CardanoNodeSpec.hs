module Test.CardanoNodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (
  findRunningCardanoNode,
  getCardanoNodeVersion,
  withCardanoNodeDevnet,
  withCardanoNodeOnKnownNetwork,
 )

import Hydra.Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic), unFile)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Cluster.Fixture (KnownNetwork (..), toNetworkId)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options (ChainBackendOptions (..), DirectOptions (..))
import Test.Hydra.Cluster.Utils (chainPointToSlot, forEachKnownNetwork)
import "directory" System.Directory (doesFileExist)

supportedNetworks :: [KnownNetwork]
supportedNetworks = [Mainnet, Preproduction, Preview]

supportedCardanoNodeVersion :: String
supportedCardanoNodeVersion = "10.6.1"

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
      withCardanoNodeDevnet tr tmp $ \blockTime backend -> do
        let nodeSocket' =
              case Backend.getOptions backend of
                Direct DirectOptions{Hydra.Options.nodeSocket} -> nodeSocket
                Blockfrost _ -> error "Unexpected Blockfrost options"
        doesFileExist (unFile nodeSocket') `shouldReturn` True
        networkId <- Backend.queryNetworkId backend
        -- NOTE: We hard-code the expected networkId and blockTime here to
        -- detect any change to the genesis-shelley.json
        networkId `shouldBe` Testnet (NetworkMagic 42)
        blockTime `shouldBe` 0.1
        -- Should produce blocks (tip advances)
        slot1 <- chainPointToSlot <$> Backend.queryTip backend
        threadDelay 1
        slot2 <- chainPointToSlot <$> Backend.queryTip backend
        slot2 `shouldSatisfy` (> slot1)

    describe "findRunningCardanoNode" $ do
      it "returns Nothing on non-matching network" $ \(tr, tmp) -> do
        withCardanoNodeOnKnownNetwork tr tmp Preview $ \_ _ -> do
          findRunningCardanoNode tr tmp Preproduction `shouldReturn` Nothing

      it "returns Just running node on matching network" $ \(tr, tmp) -> do
        withCardanoNodeOnKnownNetwork tr tmp Preview $ \blockTime backend -> do
          findRunningCardanoNode tr tmp Preview `shouldReturn` Just (blockTime, backend)

  forSupportedKnownNetworks "withCardanoNodeOnKnownNetwork starts synchronizing within 10 seconds" $ \network -> do
    -- NOTE: This implies that withCardanoNodeOnKnownNetwork does not
    -- synchronize the whole chain before continuing.
    setupTracerAndTempDir $ \(tr, tmp) ->
      withCardanoNodeOnKnownNetwork tr tmp network $ \blockTime backend -> do
        networkId <- Backend.queryNetworkId backend
        networkId `shouldBe` toNetworkId network
        blockTime `shouldBe` 20
        -- Should synchronize blocks (tip advances)
        slot1 <- chainPointToSlot <$> Backend.queryTip backend
        threadDelay 10
        slot2 <- chainPointToSlot <$> Backend.queryTip backend
        slot2 `shouldSatisfy` (> slot1)

setupTracerAndTempDir :: ToJSON msg => ((Tracer IO msg, FilePath) -> IO a) -> IO a
setupTracerAndTempDir action =
  showLogsOnFailure "CardanoNodeSpec" $ \tr ->
    withTempDir "hydra-cluster" $ \tmp ->
      action (tr, tmp)
