-- | Mutation-based script validator tests for the commit transaction where a
-- 'healthyCommitTx' gets mutated by an arbitrary 'CommitMutation'.
module Hydra.Tx.Contract.Commit where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (bodyTxL)
import Cardano.Ledger.Api.Tx.Body (EraTxBody (outputsTxBodyL), setMinCoinTxOut)
import Control.Lens (mapped, (%~))
import Data.List qualified as List
import Data.Maybe (fromJust)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.InitialError (InitialError (..))
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Tx (CommitBlueprintTx (..), Party, mkHeadId)
import Hydra.Tx.Commit (commitTx)
import Hydra.Tx.Init (mkInitialOutput)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import PlutusLedgerApi.Common (fromBuiltin)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Fixture qualified as Fixtures
import Test.Hydra.Tx.Gen (genAddressInEra, genScriptRegistry, genSigningKey, genUTxOAdaOnlyOfSize, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replacePolicyIdWith,
 )
import Test.QuickCheck (elements, oneof, scale, suchThat)

--
-- CommitTx
--

healthyCommitTx :: (Tx, UTxO)
healthyCommitTx =
  (tx', lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton healthyInitialTxIn (toCtxUTxOTxOut healthyInitialTxOut)
      <> healthyCommittedUTxO
      <> registryUTxO scriptRegistry

  tx' = fromLedgerTx . setOutputsMinValue $ toLedgerTx tx

  setOutputsMinValue =
    bodyTxL . outputsTxBodyL . mapped %~ setMinCoinTxOut Fixture.pparams

  blueprintTx = txSpendingUTxO healthyCommittedUTxO

  tx =
    commitTx
      Fixture.testNetworkId
      scriptRegistry
      (mkHeadId Fixture.testPolicyId)
      commitParty
      CommitBlueprintTx{lookupUTxO = healthyCommittedUTxO, blueprintTx}
      (healthyInitialTxIn, toCtxUTxOTxOut healthyInitialTxOut, initialPubKeyHash)
      Nothing

  scriptRegistry = genScriptRegistry `generateWith` 42

  initialPubKeyHash = verificationKeyHash commitVerificationKey

  commitParty :: Party
  commitParty = generateWith arbitrary 42

commitSigningKey :: SigningKey PaymentKey
commitSigningKey = genSigningKey `generateWith` 42

commitVerificationKey :: VerificationKey PaymentKey
commitVerificationKey = getVerificationKey commitSigningKey

healthyInitialTxIn :: TxIn
healthyInitialTxIn = generateWith arbitrary 42

healthyInitialTxOut :: TxOut CtxTx
healthyInitialTxOut =
  setMinUTxOValue Fixture.pparams . toCtxUTxOTxOut $
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
    [ SomeMutation (pure $ toErrorCode WrongHeadIdInCommitDatum) NonContinuousHeadId <$> do
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= healthyInitialTxIn))
        let mutateHeadId =
              modifyInlineDatum $
                \((party, mCommit, _headId) :: Commit.DatumType) ->
                  (party, mCommit, toPlutusCurrencySymbol otherHeadId)
        pure $ ChangeOutput 0 $ mutateHeadId commitTxOut
    , SomeMutation (pure $ toErrorCode LockedValueDoesNotMatch) MutateCommitOutputValue . ChangeOutput 0 <$> do
        let totalValueMinusOneLovelace = negateValue (lovelaceToValue 1) <> txOutValue healthyInitialTxOut <> foldMap (txOutValue . snd) (UTxO.toList healthyCommittedUTxO)
        pure $ commitTxOut{txOutValue = totalValueMinusOneLovelace}
    , SomeMutation (pure $ toErrorCode LockedValueDoesNotMatch) MutateCommittedValue <$> do
        mutatedValue <- scale (`div` 2) genValue `suchThat` (/= aCommittedOutputValue)
        let mutatedOutput = modifyTxOutValue (const mutatedValue) aCommittedTxOut
        pure $ ChangeInput aCommittedTxIn mutatedOutput Nothing
    , SomeMutation (pure $ toErrorCode MismatchCommittedTxOutInDatum) MutateCommittedAddress <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId `suchThat` (/= aCommittedAddress)
        let mutatedOutput = modifyTxOutAddress (const mutatedAddress) aCommittedTxOut
        pure $ ChangeInput aCommittedTxIn mutatedOutput Nothing
    , SomeMutation (map toErrorCode [MismatchCommittedTxOutInDatum, MissingCommittedTxOutInOutputDatum]) RecordAllCommittedUTxO <$> do
        (removedTxIn, removedTxOut) <- elements $ UTxO.toList healthyCommittedUTxO
        -- Leave out not-committed value
        let mutatedCommitTxOut = modifyTxOutValue (\v -> negateValue (txOutValue removedTxOut) <> v) commitTxOut
        pure $
          Changes
            [ RemoveInput removedTxIn
            , ChangeOutput 0 mutatedCommitTxOut
            , ChangeInput
                healthyInitialTxIn
                (toCtxUTxOTxOut healthyInitialTxOut)
                (Just $ toScriptData $ Initial.ViaCommit (removedTxIn `List.delete` allCommittedTxIn <&> toPlutusTxOutRef))
            ]
    , SomeMutation (pure $ toErrorCode MissingOrInvalidCommitAuthor) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , -- XXX: This is a bit confusing and not giving much value. Maybe we can remove this.
      -- This also seems to be covered by MutateRequiredSigner
      SomeMutation (pure $ toErrorCode CouldNotFindTheCorrectCurrencySymbolInTokens) UsePTFromDifferentHead <$> do
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= healthyInitialTxIn))
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId commitTxOut)
            , ChangeInput
                healthyInitialTxIn
                (toCtxUTxOTxOut $ replacePolicyIdWith Fixture.testPolicyId otherHeadId healthyInitialTxOut)
                (Just $ toScriptData $ Initial.ViaCommit (allCommittedTxIn <&> toPlutusTxOutRef))
            ]
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    ]
 where
  commitTxOut = fromJust $ txOuts' tx !!? 0

  allCommittedTxIn = UTxO.inputSet healthyCommittedUTxO & toList

  (aCommittedTxIn, aCommittedTxOut) = List.head $ UTxO.toList healthyCommittedUTxO

  aCommittedAddress = txOutAddress aCommittedTxOut

  aCommittedOutputValue = txOutValue aCommittedTxOut

-- | Generates value such that:
-- - alters between policy id we use in test fixtures with a random one.
-- - mixing arbitrary token names with 'hydraHeadV1'
-- - excluding 0 for quantity to mimic minting/burning
genMintedOrBurnedValue :: Gen Value
genMintedOrBurnedValue = do
  policyId <-
    oneof
      [ headPolicyId <$> arbitrary
      , pure Fixtures.testPolicyId
      ]
  tokenName <- oneof [arbitrary, pure (AssetName $ fromBuiltin hydraHeadV1)]
  quantity <- arbitrary `suchThat` (/= 0)
  pure $ fromList [(AssetId policyId tokenName, Quantity quantity)]
