module Test.LocalClusterSpec where

import Cardano.Prelude

import Lib (ClusterConfig(..), withCluster)
import Logging (Severity(..), withStdoutTracer)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it)

import qualified Data.Text as T

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should start" $ do
    withStdoutTracer "local-cluster" Info sshow $ \tr ->
      withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
        withCluster tr (ClusterConfig tmp) $ \_ -> pure ()

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
