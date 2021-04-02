{-# LANGUAGE DerivingStrategies #-}

module Hydra.NodeSpec where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.ContractStateMachine (HydraState (..), toDatumHash)
import Hydra.Node
import Test.Hspec (Expectation, Spec, around, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = around startStopNode $ do
  describe "Live Hydra Node" $ do
    it "returns Ready when asked status" $ \node -> do
      status node `shouldReturn` Ready

  describe "On-Chain Transactions" $ do
    it "Build Init transaction with outputs to be consumed by peers when given Init command" $ \node -> do
      let initParameters =
            HeadParameters{verificationKeys = [key1, key2, key3]}
          key1 = VerificationKey "key1"
          key2 = VerificationKey "key2"
          key3 = VerificationKey "key3"
          numberOfParticipants = length (verificationKeys initParameters)

      (tx, policyId) <- buildInitialTransaction node initParameters

      length (outputs tx)
        `shouldBe` 1 + numberOfParticipants

      assertValidParticipationTokens tx policyId numberOfParticipants

      assertStateMachineOutputIsInitialised tx initialState

assertStateMachineOutputIsInitialised ::
  Transaction -> HydraState -> Expectation
assertStateMachineOutputIsInitialised tx st = do
  let smOutput = getStateMachineOutput tx
  address smOutput `shouldBe` contractAddress
  datum smOutput `shouldBe` toDatumHash st

assertValidParticipationTokens :: Transaction -> PolicyId -> Int -> Expectation
assertValidParticipationTokens tx policyId numberOfParticipants = do
  length (filter (hasParticipationToken policyId 1) (outputs tx)) `shouldBe` numberOfParticipants
  Set.size (assetNames policyId tx) `shouldBe` numberOfParticipants

hasParticipationToken :: PolicyId -> Quantity -> TransactionOutput -> Bool
hasParticipationToken policyId numberOfTokens TransactionOutput{value = Value _ tokens} =
  (Map.elems <$> Map.lookup policyId tokens) == Just [numberOfTokens]
