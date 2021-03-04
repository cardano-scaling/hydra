module Test.LocalClusterSpec where

import Cardano.Prelude

import Lib (ClusterConfig(..), withCluster)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it)
import Logging (withStdoutTracer, Severity(..))

import qualified Data.Text as T

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should start" $ do
    withStdoutTracer "local-cluster" Info msgToText $ \tr ->
      withSystemTempDirectory "hydra-local-cluster" $ \tmp -> do
        withCluster tr (ClusterConfig tmp) $ \_ -> pure ()

msgToText :: Show msg => msg -> Text
msgToText = T.pack . show
