module Test.LocalClusterSpec where

import Cardano.Prelude

import Data.String (IsString (..))
import Lib (ClusterConfig (..), ClusterLog (..), RunningCluster (..), withCluster)
import Logging (Severity (..), Tracer, contramap, withStdoutTracer)
import Node (RunningNode (..), cliQueryTip)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should start" $ do
    withStdoutTracer "local-cluster" Info sshow $ \tr ->
      withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
        withCluster tr (ClusterConfig tmp) $ assertNetworkIsUp tr

assertNetworkIsUp :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsUp tracer = \case
  RunningCluster _ (RunningNode nodeId socket : _) -> do
    initialTip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
    waitForNewSlot
    anotherTip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
    anotherTip `shouldSatisfy` (> initialTip)
  _ ->
    panic "empty cluster?"

waitForNewSlot :: IO ()
waitForNewSlot = threadDelay (2 * slotLength)

slotLength :: Int -- in Microseconds
slotLength = 1_000_000 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
