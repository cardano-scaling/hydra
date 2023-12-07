-- | Mutation-based script validator tests for the commit transaction where a
-- 'healthyCommitTx' gets mutated by an arbitrary 'CommitMutation'.
module Hydra.Chain.Direct.Contract.Commit where

import Hydra.Cardano.Api
import Hydra.Prelude

-- Arbitrary VerificationKey instance
import Hydra.Chain.Direct.TxSpec ()

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (bodyTxL)
import Cardano.Ledger.Api.Tx.Body (EraTxBody (outputsTxBodyL), setMinCoinTxOut)
import Control.Lens (mapped, (%~))
import Data.List qualified as List
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Gen (genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replacePolicyIdWith,
 )
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (commitTx, mkHeadId, mkInitialOutput, verificationKeyToOnChainId)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.InitialError (InitialError (..))
import Hydra.Ledger.Cardano (
  genAddressInEra,
  genUTxOAdaOnlyOfSize,
  genValue,
  genVerificationKey,
 )
import Hydra.Party (Party)
import Test.QuickCheck (elements, oneof, scale, suchThat)

--
-- CommitTx
--

healthyCommitTx :: (Tx, UTxO)
healthyCommitTx =
  (tx', lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (healthyIntialTxIn, toUTxOContext healthyInitialTxOut)
      <> healthyCommittedUTxO
      <> registryUTxO scriptRegistry

  tx' = fromLedgerTx . setOutputsMinValue $ toLedgerTx tx

  setOutputsMinValue =
    bodyTxL . outputsTxBodyL . mapped %~ setMinCoinTxOut Fixture.pparams

  tx =
    commitTx
      Fixture.testNetworkId
      scriptRegistry
      (mkHeadId Fixture.testPolicyId)
      commitParty
      (healthyCommittedUTxO <&> (,KeyWitness KeyWitnessForSpending))
      (healthyIntialTxIn, toUTxOContext healthyInitialTxOut, initialPubKeyHash)

  scriptRegistry = genScriptRegistry `generateWith` 42

  initialPubKeyHash = verificationKeyHash commitVerificationKey

  commitParty :: Party
  commitParty = generateWith arbitrary 42

commitVerificationKey :: VerificationKey PaymentKey
commitVerificationKey = generateWith arbitrary 42

healthyIntialTxIn :: TxIn
healthyIntialTxIn = generateWith arbitrary 42

healthyInitialTxOut :: TxOut CtxTx
healthyInitialTxOut =
  setMinUTxOValue Fixture.pparams . toUTxOContext $
    mkInitialOutput Fixture.testNetworkId Fixture.testSeedInput $
      verificationKeyToOnChainId commitVerificationKey

-- NOTE: A UTxO of length 2 is picked to mutate it into cases where committing a
-- single and empty UTxO.
healthyCommittedUTxO :: UTxO
healthyCommittedUTxO =
  flip generateWith 42 $
    genUTxOAdaOnlyOfSize 2

data CommitMutation
  = -- | The headId in the output datum must match the one from the input datum.
    NonContinuousHeadId
  | -- | Invalidates the transaction by changing the committed output value.
    MutateCommitOutputValue
  | -- | Invalidates the transaction by changing the value of the committed utxo
    -- on the input side of the transaction.
    MutateCommittedValue
  | -- | Ensures the datum recording the commit is consistent with the UTxO
    -- being committed.
    MutateCommittedAddress
  | -- | Ensures a commit cannot be left out when "declared" in the commit
    -- transaction output datum.
    RecordAllCommittedUTxO
  | -- | Ensures commit is authenticated by a Head party by changing the signer
    -- used on the transaction to be the one in the PT.
    MutateRequiredSigner
  | -- | Change the head policy id to simulate commit using a PT and signer from
    -- a different head. The signer shows a correct signature but from a
    -- different head. This will cause the signer to not be present in the
    -- participation tokens.
    UsePTFromDifferentHead
  | -- | Minting or burning of the tokens should not be possible in commit.
    MutateTokenMintingOrBurning
  deriving stock (Generic, Show, Enum, Bounded)

genCommitMutation :: (Tx, UTxO) -> Gen SomeMutation
genCommitMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode WrongHeadIdInCommitDatum) NonContinuousHeadId <$> do
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= healthyIntialTxIn))
        let mutateHeadId =
              modifyInlineDatum $
                \((party, mCommit, _headId) :: Commit.DatumType) ->
                  (party, mCommit, toPlutusCurrencySymbol otherHeadId)
        pure $ ChangeOutput 0 $ mutateHeadId commitTxOut
    , SomeMutation (Just $ toErrorCode LockedValueDoesNotMatch) MutateCommitOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- scale (`div` 2) genValue `suchThat` (/= commitOutputValue)
        pure $ commitTxOut{txOutValue = mutatedValue}
    , SomeMutation (Just $ toErrorCode LockedValueDoesNotMatch) MutateCommittedValue <$> do
        mutatedValue <- scale (`div` 2) genValue `suchThat` (/= aCommittedOutputValue)
        let mutatedOutput = modifyTxOutValue (const mutatedValue) aCommittedTxOut
        pure $ ChangeInput aCommittedTxIn mutatedOutput Nothing
    , SomeMutation (Just $ toErrorCode MismatchCommittedTxOutInDatum) MutateCommittedAddress <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId `suchThat` (/= aCommittedAddress)
        let mutatedOutput = modifyTxOutAddress (const mutatedAddress) aCommittedTxOut
        pure $ ChangeInput aCommittedTxIn mutatedOutput Nothing
    , SomeMutation (Just $ toErrorCode MissingCommittedTxOutInOutputDatum) RecordAllCommittedUTxO <$> do
        (removedTxIn, removedTxOut) <- elements $ UTxO.pairs healthyCommittedUTxO
        -- Leave out not-committed value
        let mutatedCommitTxOut = modifyTxOutValue (\v -> negateValue (txOutValue removedTxOut) <> v) commitTxOut
        pure $
          Changes
            [ RemoveInput removedTxIn
            , ChangeOutput 0 mutatedCommitTxOut
            , ChangeInput
                healthyIntialTxIn
                (toUTxOContext healthyInitialTxOut)
                (Just $ toScriptData $ Initial.ViaCommit (removedTxIn `List.delete` allComittedTxIn <&> toPlutusTxOutRef))
            ]
    , SomeMutation (Just $ toErrorCode MissingOrInvalidCommitAuthor) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , -- XXX: This is a bit confusing and not giving much value. Maybe we can remove this.
      -- This also seems to be covered by MutateRequiredSigner
      SomeMutation (Just $ toErrorCode CouldNotFindTheCorrectCurrencySymbolInTokens) UsePTFromDifferentHead <$> do
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= healthyIntialTxIn))
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId commitTxOut)
            , ChangeInput
                healthyIntialTxIn
                (toUTxOContext $ replacePolicyIdWith Fixture.testPolicyId otherHeadId healthyInitialTxOut)
                (Just $ toScriptData $ Initial.ViaCommit (allComittedTxIn <&> toPlutusTxOutRef))
            ]
    , SomeMutation (Just $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    ]
 where
  TxOut{txOutValue = commitOutputValue} = commitTxOut

  commitTxOut = fromJust $ txOuts' tx !!? 0

  allComittedTxIn = UTxO.inputSet healthyCommittedUTxO & toList

  (aCommittedTxIn, aCommittedTxOut) = List.head $ UTxO.pairs healthyCommittedUTxO

  aCommittedAddress = txOutAddress aCommittedTxOut

  aCommittedOutputValue = txOutValue aCommittedTxOut
