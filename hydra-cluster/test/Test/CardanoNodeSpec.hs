module Test.CardanoNodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (HydraTestnet (..))

import CardanoNode (
  findRunningCardanoNode,
  findRunningCardanoNode',
  getCardanoNodeVersion,
  runBackend,
  withCardanoNodeDevnet,
  withCardanoNodeOnKnownNetwork,
 )
import Control.Tracer.JSON (Tracer, showLogsOnFailure)
import Hydra.Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic), unFile)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Cluster.Fixture (KnownNetwork (..), toNetworkId)
import Hydra.Options (ChainBackendOptions (..), DirectOptions (..))
import System.Directory (doesFileExist)
import Test.Hydra.Cluster.Utils (chainPointToSlot, forEachKnownNetwork)

supportedNetworks :: [KnownNetwork]
supportedNetworks = [Mainnet, Preproduction, Preview]

supportedCardanoNodeVersion :: String
supportedCardanoNodeVersion = "11.0.1"

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

  around setupTracerAndTempDir $ do
    it "withCardanoNodeDevnet does start a block-producing devnet within 5 seconds" $ \(tr, tmp) ->
      failAfter 5 $
        withCardanoNodeDevnet tr tmp $ \blockTime opts -> do
          let DirectOptions{nodeSocket = nodeSocket'} = opts
          doesFileExist (unFile nodeSocket') `shouldReturn` True
          networkId <- runBackend (Direct opts) queryNetworkId
          -- NOTE: We hard-code the expected networkId and blockTime here to
          -- detect any change to the genesis-shelley.json
          networkId `shouldBe` Testnet (NetworkMagic 42)
          blockTime `shouldBe` 0.1
          -- Should produce blocks (tip advances)
          slot1 <- chainPointToSlot <$> runBackend (Direct opts) queryTip
          threadDelay 1
          slot2 <- chainPointToSlot <$> runBackend (Direct opts) queryTip
          slot2 `shouldSatisfy` (> slot1)

    -- NOTE: These run against a local devnet instead of a public network; a
    -- known-network node needs config downloads over HTTPS, which made these
    -- time out whenever the mirrors were slow.
    describe "findRunningCardanoNode" $ do
      it "returns Nothing on non-matching network" $ \(tr, tmp) ->
        failAfter 60 $
          withCardanoNodeDevnet tr tmp $ \_ _ ->
            findRunningCardanoNode tr tmp Preproduction `shouldReturn` Nothing

      it "returns Just running node on matching network" $ \(tr, tmp) ->
        failAfter 60 $
          withCardanoNodeDevnet tr tmp $ \blockTime opts -> do
            let DirectOptions{networkId = networkId', nodeSocket = nodeSocket'} = opts
            findRunningCardanoNode' tr networkId' nodeSocket' `shouldReturn` Just (blockTime, opts)

  -- Downloads configs from public mirrors and synchronizes against live
  -- networks; external availability must not block PR CI.
  around_ onlyNightly $ forSupportedKnownNetworks "withCardanoNodeOnKnownNetwork starts synchronizing within 10 seconds @nightly" $ \network -> do
    -- NOTE: This implies that withCardanoNodeOnKnownNetwork does not
    -- synchronize the whole chain before continuing.
    setupTracerAndTempDir $ \(tr, tmp) ->
      withCardanoNodeOnKnownNetwork tr tmp network $ \blockTime opts -> do
        networkId <- runBackend (Direct opts) queryNetworkId
        networkId `shouldBe` toNetworkId network
        blockTime `shouldBe` 20
        -- Should synchronize blocks (tip advances)
        slot1 <- chainPointToSlot <$> runBackend (Direct opts) queryTip
        threadDelay 10
        slot2 <- chainPointToSlot <$> runBackend (Direct opts) queryTip
        slot2 `shouldSatisfy` (> slot1)

setupTracerAndTempDir :: ToJSON msg => ((Tracer IO msg, FilePath) -> IO a) -> IO a
setupTracerAndTempDir action =
  showLogsOnFailure "CardanoNodeSpec" $ \tr ->
    withTempDir "hydra-cluster" $ \tmp ->
      action (tr, tmp)
