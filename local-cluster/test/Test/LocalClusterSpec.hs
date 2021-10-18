module Test.LocalClusterSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (Address, ShelleyAddr, serialiseToBech32)
import CardanoClient (buildAddress)
import CardanoCluster (ClusterConfig (..), ClusterLog (..), RunningCluster (..), keysFor, testClusterConfig, withCluster)
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import Data.Text (unpack)
import Hydra.Logging (Tracer, showLogsOnFailure)
import qualified Paths_local_cluster as Pkg
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process

spec :: Spec
spec =
  it "should produce blocks and provide funds" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-local-cluster" $ \tmp -> do
        let config = testClusterConfig tmp
        withCluster tr config $ \cluster -> do
          failAfter 30 $ assertNetworkIsProducingBlock tr cluster
          assertCanSpendInitialFunds cluster

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

assertCanSpendInitialFunds :: HasCallStack => RunningCluster -> IO ()
assertCanSpendInitialFunds = \case
  cluster@(RunningCluster ClusterConfig{parentStateDirectory, networkId} (RunningNode nodeId socket : _)) -> do
    (vk, _) <- keysFor "alice" cluster
    addr <- buildAddress vk networkId
    runTestScript (parentStateDirectory </> "node-" <> show nodeId) addr socket
  _ ->
    error "empty cluster?"

runTestScript :: FilePath -> Address ShelleyAddr -> FilePath -> IO ()
runTestScript nodeDirectory addr socket = do
  inputScript <- Pkg.getDataFileName "test_submit.sh"
  currentEnv <- getEnvironment
  let scriptOutput = nodeDirectory </> "test_submit.out"
  withFile' scriptOutput $ \fileOut ->
    withCreateProcess (sh currentEnv inputScript fileOut) $ \_stdin _stdout _stderr hdl ->
      waitForProcess hdl >>= \case
        ExitFailure{} -> readFile scriptOutput >>= failure . ("Initial funds spending script failed, output: " <>)
        ExitSuccess -> pure ()
 where
  socketEnv = ("CARDANO_NODE_SOCKET_PATH", socket)
  sh baseEnv script out =
    (proc "/bin/sh" [script, unpack $ serialiseToBech32 addr])
      { env = Just (socketEnv : baseEnv)
      , cwd = Just nodeDirectory
      , std_out = UseHandle out
      , std_err = UseHandle out
      }

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
