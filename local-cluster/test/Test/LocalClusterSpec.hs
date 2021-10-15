module Test.LocalClusterSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoCluster (ClusterConfig (..), ClusterLog (..), RunningCluster (..), withCluster)
import CardanoNode (ChainTip (..), NodeId, RunningNode (..), cliQueryTip)
import Hydra.Logging (Tracer, showLogsOnFailure)

spec :: Spec
spec =
  it "should produce blocks" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-local-cluster" $ \tmp -> do
        withCluster tr (ClusterConfig tmp) $ \cluster -> do
          failAfter 30 $ assertNetworkIsProducingBlock tr cluster


assertNetworkIsProducingBlock :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsProducingBlock tracer = go (-1)
 where
  go blk cluster = case cluster of
    RunningCluster _ (RunningNode nodeId socket : _) -> do
      waitForNewBlock
      tip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
      if block tip > blk
        then pure ()
        else go (block tip) cluster
    _ ->
      error "empty cluster?"

  _ ->
    error "empty cluster?"

runTestScript :: NodeId -> FilePath -> IO (ExitCode, String, String)
runTestScript _nodeId _socket =
  readCreateProcessWithExitCode sh (Text.unpack inputScript)
 where
  sh = proc "/bin/sh" []
  inputScript = unlines ["exit 1 ;"]

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
