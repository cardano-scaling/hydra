{-# LANGUAGE DuplicateRecordFields #-}

module Test.Hydra.Cluster.FaucetSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (withCardanoNodeDevnet)
import Hydra.Cardano.Api (Coin (..), lovelaceToValue, selectLovelace)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, returnFundsToFaucet, seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Scenarios (EndToEndLog (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Test.Hydra.Tx.Gen (genVerificationKey)
import Test.QuickCheck (choose, elements, forAll, generate, withMaxSuccess)
import "async" Control.Concurrent.Async (replicateConcurrently)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO

setupDevnet :: ((Tracer IO FaucetLog, DirectBackend) -> IO a) -> IO a
setupDevnet action =
  failAfter 30 $
    showLogsOnFailure "FaucetSpec" $ \tracer ->
      withTempDir "hydra-cluster" $ \tmpDir ->
        withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \_ backend ->
          action (contramap FromFaucet tracer, backend)

spec :: Spec
spec =
  around setupDevnet $ do
    describe "seedFromFaucet" $
      it "should work concurrently when called multiple times with the same amount of lovelace" $ \(tracer, backend) -> do
        utxos <- replicateConcurrently 10 $ do
          vk <- generate genVerificationKey
          seedFromFaucet backend vk (lovelaceToValue 1_000_000) tracer
        -- 10 unique outputs
        UTxO.size (fold utxos) `shouldBe` 10

    describe "returnFundsToFaucet" $ do
      it "does nothing if nothing to return" $ \(tracer, backend) -> do
        returnFundsToFaucet tracer backend Alice

      it "seedFromFaucet and returnFundsToFaucet should work together" $ \(tracer, backend) -> do
        withMaxSuccess 10 $
          forAll (Coin <$> choose (1000000, 10000000000)) $ \coin ->
            forAll (elements [Alice, Bob, Carol]) $ \actor -> do
              (vk, _) <- keysFor actor
              (faucetVk, _) <- keysFor Faucet
              initialFaucetFunds <- Backend.queryUTxOFor backend QueryTip faucetVk
              void $ seedFromFaucet backend vk (lovelaceToValue coin) tracer
              returnFundsToFaucet tracer backend actor
              remaining <- Backend.queryUTxOFor backend QueryTip vk
              finalFaucetFunds <- Backend.queryUTxOFor backend QueryTip faucetVk
              UTxO.totalValue remaining `shouldBe` mempty

              -- check the faucet has one utxo extra in the end
              UTxO.size finalFaucetFunds `shouldBe` UTxO.size initialFaucetFunds + 1

              let initialFaucetValue = selectLovelace (UTxO.totalValue initialFaucetFunds)
              let finalFaucetValue = selectLovelace (UTxO.totalValue finalFaucetFunds)
              let difference = initialFaucetValue - finalFaucetValue
              -- difference between starting faucet amount and final one should
              -- just be the amount of paid fees
              difference `shouldSatisfy` (< 400_000)

    describe "publishHydraScriptsAs" $ do
      it "squash the UTxO to get a suitable output" $ \(tracer, backend) -> do
        -- NOTE: Note use 'Faucet' as this has a very big initial amount
        (vk, _) <- keysFor Alice
        -- NOTE: 83 ADA is just enough to pay for reference scripts deposits.
        forM_ [1_000_000, 2_000_000, 83_000_000] $ \c -> seedFromFaucet backend vk (lovelaceToValue c) tracer

        void $ publishHydraScriptsAs backend Alice

        -- it squashed the UTxO
        utxoAfter <- Backend.queryUTxOFor backend QueryTip vk

        UTxO.size utxoAfter `shouldBe` 1
