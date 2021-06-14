module Test.LocalClusterSpec where

import Cardano.Prelude

import Data.String (IsString (..))
import Lib (ClusterConfig (..), ClusterLog (..), RunningCluster (..), withCluster)
import Logging (Severity (..), Tracer, contramap, withTVarTracer)
import Node (ChainTip (..), RunningNode (..), cliQueryTip)
import Say (say)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should produce blocks" $ do
    withTVarTracer "local-cluster" Info sshow $ \(tr, getLogs) -> traceOnFailure getLogs $
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
    panic "empty cluster?"

traceOnFailure :: Show msg => IO [msg] -> IO a -> IO a
traceOnFailure getLogs action = do
  action `onException` (getLogs >>= say . sshow)

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: Int -- in Microseconds
slotLength = 1_000_000 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
