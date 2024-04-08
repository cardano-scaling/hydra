module Test.Hydra.Cluster.FaucetSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (RunningNode (..))
import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent.Async (replicateConcurrently)
import Hydra.Cardano.Api (Coin (..), selectLovelace, txOutValue)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryUTxOFor)
import Hydra.Cluster.Faucet (returnFundsToFaucet, seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Scenarios (EndToEndLog (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.Logging (showLogsOnFailure)
import Test.QuickCheck (choose, elements, forAll, generate, withMaxSuccess)

spec :: Spec
spec = do
  describe "seedFromFaucet" $
    it "should work concurrently when called multiple times with the same amount of lovelace" $
      showLogsOnFailure "FaucetSpec" $ \tracer ->
        failAfter 30 $
          withTempDir "hydra-cluster" $ \tmpDir ->
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              utxos <- replicateConcurrently 10 $ do
                vk <- generate genVerificationKey
                seedFromFaucet node vk 1_000_000 (contramap FromFaucet tracer)
              -- 10 unique outputs
              length (fold utxos) `shouldBe` 10
  describe "returnFundsToFaucet" $
    prop "seedFromFaucet and returnFundsToFaucet should work together" $
      withMaxSuccess 10 $
        forAll (Coin <$> choose (1000000, 10000000000)) $ \coin -> do
          forAll (elements [Alice, Bob, Carol]) $ \actor -> do
            showLogsOnFailure "FaucetSpec" $ \tracer ->
              withTempDir "hydra-cluster" $ \tmpDir ->
                withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{networkId, nodeSocket} -> do
                  let faucetTracer = contramap FromFaucet tracer
                  (vk, _) <- keysFor actor
                  (faucetVk, _) <- keysFor Faucet
                  initialFaucetFunds <- queryUTxOFor networkId nodeSocket QueryTip faucetVk
                  void $ seedFromFaucet node vk coin faucetTracer
                  returnFundsToFaucet faucetTracer node actor
                  remaining <- queryUTxOFor networkId nodeSocket QueryTip vk
                  finalFaucetFunds <- queryUTxOFor networkId nodeSocket QueryTip faucetVk
                  foldMap txOutValue remaining `shouldBe` mempty

                  -- check the faucet has one utxo extra in the end
                  length finalFaucetFunds `shouldBe` length initialFaucetFunds + 1

                  let initialFaucetValue = selectLovelace (foldMap txOutValue initialFaucetFunds)
                  let finalFaucetValue = selectLovelace (foldMap txOutValue finalFaucetFunds)
                  let difference = initialFaucetValue - finalFaucetValue
                  -- difference between starting faucet amount and final one should
                  -- just be the amount of paid fees
                  difference `shouldSatisfy` (< 340_000)
