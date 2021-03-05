module Test.LocalClusterSpec where


import Cardano.Prelude

import Data.String (IsString(..))
import Lib (ClusterConfig(..), RunningCluster(..), withCluster, ClusterLog)
import Logging (Severity(..), withStdoutTracer, Tracer)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should start" $ do
    withStdoutTracer "local-cluster" Info sshow $ \tr ->
      withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
        withCluster tr (ClusterConfig tmp) $ assertNetworkIsUp tr

assertNetworkIsUp :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsUp _ _ = panic "cluster has no running node"

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
