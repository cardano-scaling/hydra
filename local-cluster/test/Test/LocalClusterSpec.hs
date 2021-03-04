module Test.LocalClusterSpec where

import Cardano.Prelude

import Lib (ClusterConfig(..), withCluster)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should start" $ do
    withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
      withCluster (ClusterConfig tmp) $ \_ -> pure ()
