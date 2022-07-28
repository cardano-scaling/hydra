module Test.Hydra.Cluster.FaucetSpec where

import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent.Async (replicateConcurrently_)
import Hydra.Cluster.Faucet (Marked (Normal), seedFromFaucet_)
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Prelude
import HydraNode (EndToEndLog (FromCardanoNode))
import Test.Hydra.Prelude
import Test.QuickCheck (generate)

spec :: Spec
spec = around showLogsOnFailure $ do
  describe "seed from faucet" $ do
    it "should work concurrently" $ \tracer -> do
      failAfter 30 $
        withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
            replicateConcurrently_ 10 $ do
              vk <- generate genVerificationKey
              seedFromFaucet_ node vk 1_000_000 Normal
