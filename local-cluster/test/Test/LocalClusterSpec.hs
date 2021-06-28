module Test.LocalClusterSpec where

import Hydra.Prelude

import CardanoCluster (ClusterConfig (..), ClusterLog (..), RunningCluster (..), withCluster)
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import Hydra.Logging (Tracer, showLogsOnFailure)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should produce blocks" $ do
    showLogsOnFailure $ \tr ->
      withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
        withCluster tr (ClusterConfig tmp) $ assertNetworkIsProducingBlock tr

assertNetworkIsProducingBlock :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsProducingBlock tracer = \case
  RunningCluster _ (RunningNode nodeId socket : _) -> do
    initialTip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
    waitForNewBlock
    anotherTip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
    on (>) block anotherTip initialTip `shouldBe` True
  _ ->
    error "empty cluster?"

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
