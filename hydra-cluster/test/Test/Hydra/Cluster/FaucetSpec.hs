module Test.Hydra.Cluster.FaucetSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Control.Concurrent.Async (replicateConcurrently_)
import Hydra.Cardano.Api (txOutValue)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryUTxOFor)
import Hydra.Cluster.Faucet (Marked (Normal), returnFundsToFaucet, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (EndToEndLog (FromCardanoNode, FromFaucet))
import Test.QuickCheck (elements, generate)

spec :: Spec
spec = do
  describe "seedFromFaucet" $
    it "should work concurrently" $
      showLogsOnFailure $ \tracer ->
        failAfter 30 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir ->
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
              replicateConcurrently_ 10 $ do
                vk <- generate genVerificationKey
                seedFromFaucet_ node vk 1_000_000 Normal (contramap FromFaucet tracer)

  describe "returnFundsToFaucet" $
    it "seedFromFaucet and returnFundsToFaucet work together" $ do
      showLogsOnFailure $ \tracer ->
        withTempDir "end-to-end-cardano-node" $ \tmpDir ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{networkId, nodeSocket} -> do
            let faucetTracer = contramap FromFaucet tracer
            actor <- generate $ elements [Alice, Bob, Carol]
            (vk, _) <- keysFor actor
            _seeded <- seedFromFaucet node vk 100_000_000 Normal faucetTracer
            returnFundsToFaucet faucetTracer node actor
            remaining <- queryUTxOFor networkId nodeSocket QueryTip vk
            -- TODO: check remaining funds for actor and faucet
            foldMap txOutValue remaining `shouldBe` mempty
