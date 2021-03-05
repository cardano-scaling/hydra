module Test.LocalClusterSpec where

import Cardano.Prelude

import Data.String (IsString(..))
import Lib (ClusterConfig(..), RunningCluster, withCluster)
import Logging (Severity(..), withStdoutTracer)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should start" $ do
    withStdoutTracer "local-cluster" Info sshow $ \tr ->
      withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
        withCluster tr (ClusterConfig tmp) assertNetworkIsUp

assertNetworkIsUp :: RunningCluster -> IO ()
assertNetworkIsUp = panic "not implemented"

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
