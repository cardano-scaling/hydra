module Test.Hydra.Cluster.FaucetSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (RunningNode (..))
import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent.Async (replicateConcurrently_)
import Hydra.Cardano.Api (AssetId (AdaAssetId), selectAsset, txOutValue)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryUTxOFor)
import Hydra.Cluster.Faucet (returnFundsToFaucet, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Scenarios (EndToEndLog (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano (genKeyPair, genVerificationKey)
import Hydra.Logging (showLogsOnFailure)
import Test.QuickCheck (elements, generate)

spec :: Spec
spec = do
  describe "seedFromFaucet" $ do
    it "should work concurrently" $
      showLogsOnFailure "FaucetSpec" $ \tracer ->
        failAfter 30 $
          withTempDir "hydra-cluster" $ \tmpDir ->
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
              replicateConcurrently_ 10 $ do
                vk <- generate genVerificationKey
                seedFromFaucet_ node vk 1_000_000 (contramap FromFaucet tracer)
    it "should work when called multiple times with the same amount of lovelace" $
      showLogsOnFailure "FaucetSpec" $ \tracer ->
        failAfter 30 $
          withTempDir "hydra-cluster" $ \tmpDir ->
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              walletVk <- fst <$> generate genKeyPair
              uTxO1 <- seedFromFaucet node walletVk 2_000_000 (contramap FromFaucet tracer)
              uTxO2 <- seedFromFaucet node walletVk 2_000_000 (contramap FromFaucet tracer)
              uTxO1 `shouldNotBe` uTxO2
  describe "returnFundsToFaucet" $
    it "seedFromFaucet and returnFundsToFaucet should work together" $ do
      showLogsOnFailure "FaucetSpec" $ \tracer ->
        withTempDir "hydra-cluster" $ \tmpDir ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{networkId, nodeSocket} -> do
            let faucetTracer = contramap FromFaucet tracer
            actor <- generate $ elements [Alice, Bob, Carol]
            (vk, _) <- keysFor actor
            (faucetVk, _) <- keysFor Faucet
            initialFaucetFunds <- queryUTxOFor networkId nodeSocket QueryTip faucetVk
            seeded <- seedFromFaucet node vk 100_000_000 faucetTracer
            returnFundsToFaucet faucetTracer node actor
            remaining <- queryUTxOFor networkId nodeSocket QueryTip vk
            finalFaucetFunds <- queryUTxOFor networkId nodeSocket QueryTip faucetVk
            foldMap txOutValue remaining `shouldBe` mempty

            -- check the faucet has one utxo extra in the end
            length seeded `shouldBe` length remaining + 1

            let initialFaucetValue = selectAsset (foldMap txOutValue initialFaucetFunds) AdaAssetId
            let finalFaucetValue = selectAsset (foldMap txOutValue finalFaucetFunds) AdaAssetId
            let difference = initialFaucetValue - finalFaucetValue
            -- difference between starting faucet amount and final one should
            -- just be the amount of paid fees
            difference `shouldSatisfy` (< 340_000)
