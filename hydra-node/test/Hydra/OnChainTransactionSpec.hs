{-# LANGUAGE DerivingStrategies #-}

module Hydra.OnChainTransactionSpec where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hydra.ContractStateMachine (HydraState (..), VerificationKey (..), contractAddress, toDatumHash)
import Hydra.MonetaryPolicy (hydraCurrencySymbol)
import Hydra.OnChainTransaction
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import qualified Prelude

spec :: Spec
spec = describe "On-Chain Transactions" $ do
  describe "Initial transaction" $ do
    it "Build Init transaction with outputs to be consumed by peers given head parameters" $ do
      let initParameters =
            HeadParameters{verificationKeys = [key1, key2, key3], monetaryPolicyInput = mkTransactionInput "1" 1}
          key1 = VerificationKey "key1"
          key2 = VerificationKey "key2"
          key3 = VerificationKey "key3"
          numberOfParticipants = length (verificationKeys initParameters)

          (tx, policyId) = buildInitialTransaction initParameters

      length (outputs tx)
        `shouldBe` 1 + numberOfParticipants

      assertValidParticipationTokens tx policyId numberOfParticipants

      assertStateMachineOutputIsInitialised
        tx
        (initialState $ verificationKeys initParameters)

      assertTransactionInputValidatesMonetaryPolicy tx policyId

assertTransactionInputValidatesMonetaryPolicy :: Transaction -> PolicyId -> Expectation
assertTransactionInputValidatesMonetaryPolicy tx policyId = do
  let txInputs = inputs tx
      txOutputRef = outputRef $ Prelude.head txInputs
  length txInputs `shouldBe` 1
  toCurrencySymbol policyId `shouldBe` hydraCurrencySymbol txOutputRef

assertStateMachineOutputIsInitialised ::
  Transaction -> HydraState -> Expectation
assertStateMachineOutputIsInitialised tx st = do
  let smOutput = fromJust $ getStateMachineOutput tx
  toPlutusAddress (address smOutput) `shouldBe` Just contractAddress
  datum smOutput `shouldBe` Just (toDatumHash st)

assertValidParticipationTokens :: Transaction -> PolicyId -> Int -> Expectation
assertValidParticipationTokens tx policyId numberOfParticipants = do
  length (filter (hasParticipationToken policyId 1) (outputs tx)) `shouldBe` numberOfParticipants
  Set.size (assetNames policyId tx) `shouldBe` numberOfParticipants

hasParticipationToken :: PolicyId -> Quantity -> TransactionOutput -> Bool
hasParticipationToken policyId numberOfTokens TransactionOutput{value = Value _ tokens} =
  (Map.elems <$> Map.lookup policyId tokens) == Just [numberOfTokens]
