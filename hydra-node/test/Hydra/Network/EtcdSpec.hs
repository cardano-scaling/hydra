-- | Tests of the etcd-backed network component.
module Hydra.Network.EtcdSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Network.Etcd (NetworkConfigurationMismatch (..), checkClusterPeers)

spec :: Spec
spec = do
  describe "checkClusterPeers" $ do
    it "succeeds on first run" $
      withTempDir "etcd-cluster-peers" $ \dir ->
        checkClusterPeers dir "alice=http://a,bob=http://b"

    it "succeeds when re-run with the same peers" $
      withTempDir "etcd-cluster-peers" $ \dir -> do
        checkClusterPeers dir "alice=http://a,bob=http://b"
        checkClusterPeers dir "alice=http://a,bob=http://b"

    it "fails when re-run with a changed peer configuration" $
      withTempDir "etcd-cluster-peers" $ \dir -> do
        checkClusterPeers dir "alice=http://a,bob=http://b"
        checkClusterPeers dir "alice=http://a,carol=http://c"
          `shouldThrow` \NetworkConfigurationMismatch{} -> True
