module Test.Hydra.Cluster.FaucetSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent.Async (replicateConcurrently_)
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet_)
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.Logging (showLogsOnFailure)
import Test.QuickCheck (generate)

spec :: Spec
spec =
  describe "seed from faucet" $
    it "should work concurrently" $
      showLogsOnFailure $ \tracer ->
        failAfter 30 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir ->
            withCardanoNodeDevnet tracer tmpDir $ \node ->
              replicateConcurrently_ 10 $ do
                vk <- generate genVerificationKey
                seedFromFaucet_ node vk 1_000_000 Normal
