{-# LANGUAGE DerivingStrategies #-}

module Hydra.OnChainTransactionSpec where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hydra.Arbitraries (SomeHeadParameters (..))
import Hydra.ContractStateMachine (VerificationKey (..), contractAddress, toDatumHash)
import Hydra.MonetaryPolicy (hydraCurrencySymbol)
import Hydra.OnChainTransaction
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
import qualified Prelude

spec :: Spec
spec = describe "On-Chain Transactions" $ do
  describe "Initial transaction" $ do
    it "Build Init transaction with outputs to be consumed by peers given head parameters" $
      property buildsValidInitialTransaction

buildsValidInitialTransaction ::
  SomeHeadParameters -> Bool
buildsValidInitialTransaction (SomeHeadParameters initParameters@HeadParameters{verificationKeys}) =
  let numberOfParticipants = length verificationKeys
      (tx, policyId) = buildInitialTransaction initParameters
   in length (outputs tx) == (1 + numberOfParticipants)
        && participationTokensAreUnique tx policyId numberOfParticipants
        && stateMachineOutputIsInitialised tx verificationKeys
        && transactionInputValidatesMonetaryPolicy tx policyId

transactionInputValidatesMonetaryPolicy :: Transaction -> MonetaryPolicyId -> Bool
transactionInputValidatesMonetaryPolicy tx policyId =
  let txInputs = inputs tx
      txOutputRef = outputRef $ Prelude.head txInputs
   in length txInputs == 1
        && toCurrencySymbol policyId == hydraCurrencySymbol (first toTxId txOutputRef)

stateMachineOutputIsInitialised ::
  Transaction -> [VerificationKey] -> Bool
stateMachineOutputIsInitialised tx keys =
  let smOutput = fromJust $ getStateMachineOutput tx
   in toPlutusAddress (address smOutput) == contractAddress
        && (toPlutusDatumHash <$> datum smOutput) == Just (toDatumHash $ initialState keys)

participationTokensAreUnique :: Transaction -> MonetaryPolicyId -> Int -> Bool
participationTokensAreUnique tx policyId numberOfParticipants =
  length (filter (hasParticipationToken policyId 1) (outputs tx)) == numberOfParticipants
    && Set.size (assetNames policyId tx) == numberOfParticipants

hasParticipationToken :: MonetaryPolicyId -> Quantity -> TransactionOutput -> Bool
hasParticipationToken policyId numberOfTokens TransactionOutput{value = Value _ tokens} =
  (Map.elems <$> Map.lookup policyId tokens) == Just [numberOfTokens]
