module Test.CardanoClusterSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (queryTip)
import CardanoCluster (
  Actor (Alice),
  ClusterConfig (..),
  ClusterLog (..),
  Marked (Normal),
  RunningCluster (..),
  defaultNetworkId,
  keysFor,
  seedFromFaucet_,
  withCluster,
 )
import CardanoNode (RunningNode (..))
import Hydra.Cardano.Api (ChainPoint (..))
import Hydra.Logging (Tracer, showLogsOnFailure)

spec :: Spec
spec =
  it "should produce blocks, provide funds, and send Hydra OCV transactions" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-cluster" $ \tmp -> do
        let config =
              ClusterConfig
                { parentStateDirectory = tmp
                , networkId = defaultNetworkId
                }
        withCluster tr config $ \cluster -> do
          failAfter 30 $ assertNetworkIsProducingBlock tr cluster
          failAfter 30 $ assertCanSpendInitialFunds cluster

assertNetworkIsProducingBlock :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsProducingBlock _ = \case
  (RunningCluster _ (RunningNode _ socket : _)) -> go socket
  _ -> error "empty cluster?"
 where
  go socket = do
    waitForNewBlock
    queryTip defaultNetworkId socket >>= \case
      ChainPointAtGenesis -> go socket
      ChainPoint{} -> pure ()

assertCanSpendInitialFunds :: HasCallStack => RunningCluster -> IO ()
assertCanSpendInitialFunds = \case
  (RunningCluster ClusterConfig{networkId} (node : _)) -> do
    (vk, _) <- keysFor Alice
    seedFromFaucet_ networkId node vk 100_000_000 Normal
  _ ->
    error "empty cluster?"

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
