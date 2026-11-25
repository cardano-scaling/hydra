{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.InitSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Maybe (fromJust)
import Hydra.Cardano.Api (AssetId (AssetId), AssetName (UnsafeAssetName), modifyTxOutValue, txOuts')
import Hydra.Contract.HeadState (OpenDatum (..), State (..))
import Hydra.Tx.Contract.Init (healthyInitTx)
import Hydra.Tx.Init (NotAnInitReason (..), observeInitTx)
import Hydra.Tx.Observe (HeadObservation (NoHeadTx), observeHeadTxWithReason)
import Test.Hydra.Tx.Fixture (testNetworkId, testPolicyId)
import Test.Hydra.Tx.Mutation (Mutation (ChangeOutput), applyMutation, modifyInlineDatum)

spec :: Spec
spec = describe "initial Head observation" $ do
  it "accepts the constructed initial datum and value" $
    observeInitTx healthy `shouldSatisfy` isRight

  forM_ [-1, 1, 42] $ \v ->
    it ("rejects a noninitial version: " <> show v) $
      rejects (changeDatum $ \st -> st{version = v}) InvalidInitialVersion

  forM_ [-1, 0, 1] $ \overhead ->
    it ("rejects an overhead different from the output's ADA: " <> show overhead) $
      rejects (changeDatum $ \st -> st{headAdaOverhead = overhead}) InvalidInitialHeadValue

  it "rejects an initial accumulator that does not commit to the empty set" $
    rejects (changeDatum $ \st -> st{accumulatorHash = "not-the-empty-accumulator"}) InvalidInitialAccumulator

  it "rejects additional head value absent from the empty initial snapshot" $
    rejects (modifyTxOutValue (<> fromList [(AssetId testPolicyId (UnsafeAssetName "extra"), 1)]) headOutput) InvalidInitialHeadValue
 where
  (healthy, _) = healthyInitTx
  headOutput = fromJust $ txOuts' healthy !!? 0
  changeDatum f = modifyInlineDatum (\case Open st -> Open $ f st; other -> other) headOutput
  rejects output reason = do
    let (mutated, _) = applyMutation (ChangeOutput 0 output) healthyInitTx
    observeInitTx mutated `shouldBe` Left reason
    observeHeadTxWithReason testNetworkId mempty mutated `shouldBe` (NoHeadTx, Just reason)
