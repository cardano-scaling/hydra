{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.CloseUnused where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-plutus" Hydra.Plutus.Gen ()
import "hydra-prelude" Hydra.Prelude hiding (label)
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (arbitrarySizedNatural, choose, elements, listOf1, oneof, resize, suchThat)
import "base" Data.Maybe (fromJust)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-plutus" Hydra.Contract.Error (toErrorCode)
import "hydra-plutus" Hydra.Contract.HeadError (HeadError (..))
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-plutus" Hydra.Contract.HeadTokens (headPolicyId)
import "hydra-plutus" Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import "hydra-plutus-extras" Hydra.Plutus.Extras (posixFromUTCTime)
import "hydra-plutus-extras" Hydra.Plutus.Orphans ()
import "hydra-tx" Hydra.Tx (Snapshot (..), hashUTxO, mkHeadId, registryUTxO)
import "hydra-tx" Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import "hydra-tx" Hydra.Tx.Contract.Close.Healthy (
  healthyCloseLowerBoundSlot,
  healthyCloseUpperBoundPointInTime,
  healthyConfirmedSnapshot,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyContestationPeriodSeconds,
  healthyOnChainParties,
  healthyOpenHeadTxIn,
  healthyOpenHeadTxOut,
  healthySignature,
  healthySplitUTxOInHead,
  healthySplitUTxOToDecommit,
  somePartyCardanoVerificationKey,
 )
import "hydra-tx" Hydra.Tx.Contract.Commit (genMintedOrBurnedValue)
import "hydra-tx" Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)
import "hydra-tx" Hydra.Tx.Snapshot (getSnapshot)
import "hydra-tx" Hydra.Tx.Snapshot qualified as Snapshot
import "hydra-tx" Hydra.Tx.Utils (IncrementalAction (..), setIncrementalActionMaybe)
import "hydra-tx" Test.Hydra.Tx.Fixture qualified as Fixture
import "hydra-tx" Test.Hydra.Tx.Gen (
  genAddressInEra,
  genHash,
  genScriptRegistry,
  genValue,
  genVerificationKey,
 )
import "hydra-tx" Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replaceContestationDeadline,
  replaceContestationPeriod,
  replaceContesters,
  replaceHeadId,
  replaceOmegaUTxOHash,
  replaceParties,
  replacePolicyIdWith,
  replaceSnapshotNumber,
  replaceSnapshotVersion,
  replaceUTxOHash,
 )
import "plutus-ledger-api" PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), fromMilliSeconds)
import "plutus-ledger-api" PlutusLedgerApi.V3 (POSIXTime, PubKeyHash (PubKeyHash), toBuiltin)
import "quickcheck-instances" Test.QuickCheck.Instances ()

healthyCurrentSnapshotNumber :: Snapshot.SnapshotNumber
healthyCurrentSnapshotNumber = 1

healthyCurrentSnapshotVersion :: Snapshot.SnapshotVersion
healthyCurrentSnapshotVersion = 1

-- | Healthy close transaction for the generic case were we close a head
--  after one or more snapshot have been agreed upon between the members.
healthyCloseCurrentTx :: (Tx, UTxO)
healthyCloseCurrentTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId Fixture.testPolicyId)
      healthyCurrentSnapshotVersion
      closeUnusedSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      incrementalAction

  closeUnusedSnapshot = healthyConfirmedSnapshot healthyCurrentSnapshot

  incrementalAction =
    fromMaybe NoThing $
      setIncrementalActionMaybe (utxoToCommit $ getSnapshot closeUnusedSnapshot) (utxoToDecommit $ getSnapshot closeUnusedSnapshot)

  datum :: TxOutDatum CtxUTxO
  datum = mkTxOutDatumInline healthyCurrentOpenDatum

  lookupUTxO =
    UTxO.singleton healthyOpenHeadTxIn (healthyOpenHeadTxOut datum)
      <> registryUTxO scriptRegistry

  scriptRegistry = genScriptRegistry `generateWith` 42

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut datum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }

healthyCurrentSnapshot :: Snapshot Tx
healthyCurrentSnapshot =
  Snapshot
    { headId = mkHeadId Fixture.testPolicyId
    , version = healthyCurrentSnapshotVersion
    , number = healthyCurrentSnapshotNumber
    , confirmed = []
    , utxo = healthySplitUTxOInHead
    , utxoToCommit = Nothing
    , utxoToDecommit = Just healthySplitUTxOToDecommit
    }

healthyCurrentOpenDatum :: Head.State
healthyCurrentOpenDatum =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , utxoHash = toBuiltin $ hashUTxO @Tx healthySplitUTxOInHead
      , contestationPeriod = healthyContestationPeriod
      , headId = toPlutusCurrencySymbol Fixture.testPolicyId
      , version = toInteger healthyCurrentSnapshotVersion
      }

data CloseMutation
  = -- | Ensures collectCom does not allow any output address but νHead.
    NotContinueContract
  | -- | Ensures the snapshot signature is multisigned by all valid Head
    -- participants.
    --
    -- Invalidates the tx by changing the redeemer signature
    -- but not the snapshot number in output head datum.
    MutateSignatureButNotSnapshotNumber
  | -- | Ensures the snapshot number is consistent with the signature.
    --
    -- Invalidates the tx by changing the snapshot number
    -- in resulting head output but not the redeemer signature.
    MutateSnapshotNumberButNotSignature
  | -- | Check the snapshot version is preserved from last open state.
    MutateSnapshotVersion
  | -- | Ensures the close snapshot is multisigned by all Head participants by
    -- changing the parties in the input head datum. If they do not align the
    -- multisignature will not be valid anymore.
    SnapshotNotSignedByAllParties
  | -- | Ensures close is authenticated by a one of the Head members by changing
    -- the signer used on the tx to not be one of PTs.
    MutateRequiredSigner
  | -- | Ensures close is authenticated by a one of the Head members by changing
    -- the signer used on the tx to be empty.
    MutateNoRequiredSigner
  | -- | Ensures close is authenticated by a one of the Head members by changing
    -- the signer used on the tx to have multiple signers (including the signer
    -- to not fail for SignerIsNotAParticipant).
    MutateMultipleRequiredSigner
  | -- | Invalidates the tx by changing the utxo hash in resulting head output.
    --
    -- Ensures the output state is consistent with the redeemer.
    MutateCloseUTxOHash
  | -- | Invalidates the tx by changing the utxo to decommit hash in resulting head output.
    --
    -- Ensures the output state is consistent with the redeemer.
    MutateCloseUTxOToDecommitHash
  | -- | Ensures parties do not change between head input datum and head output datum.
    MutatePartiesInOutput
  | -- | Ensures headId do not change between head input datum and head output
    -- datum.
    MutateHeadIdInOutput
  | -- | Invalidates the tx by changing the lower bound to be non finite.
    MutateInfiniteLowerBound
  | -- | Invalidates the tx by changing the upper bound to be non finite.
    MutateInfiniteUpperBound
  | -- | Invalidates the tx by changing the contestation deadline to not satisfy
    -- `contestationDeadline = upperBound + contestationPeriod`.
    MutateContestationDeadline
  | -- | Invalidates the tx by changing the lower and upper bound to be not
    -- bounded as per spec `upperBound - lowerBound <= contestationPeriod`.
    --
    -- This also changes the resulting `head output` contestation deadline to be
    -- valid, so it satisfy `contestationDeadline = upperBound +
    -- contestationPeriod`.
    MutateValidityInterval
  | -- | Ensure the Head cannot be closed with correct authentication from a
    -- different Head. We simulate this by changing the head policy id of the ST
    -- and PTs to be of a different head - a real attack would be to add inputs
    -- with those tokens on top of spending the head output, a bit like a double
    -- satisfaction attack. Note that the token name stays the same and
    -- consistent with the signer. This will cause authentication failure
    -- because the signer's PT, although with a consistent name, is not from the
    -- right head (has a different policy id than in the datum).
    CloseFromDifferentHead
  | -- | Minting or burning of tokens should not be possible in close.
    MutateTokenMintingOrBurning
  | -- | Invalidates the tx by changing the contesters to be non empty.
    MutateContesters
  | -- | Invalidates the tx by changing output values arbitrarily to be different
    -- (not preserved) from the head.
    --
    -- Ensures values are preserved between head input and output.
    MutateValueInOutput
  | -- | Invalidate the tx by changing the contestation period.
    MutateContestationPeriod
  deriving stock (Generic, Show, Enum, Bounded)

genCloseCurrentMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseCurrentMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        signature <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $ Head.Close Head.CloseUnusedDec{signature}
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (> healthyCurrentSnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , -- Last known open state version is recorded in closed state
      SomeMutation (pure $ toErrorCode MustNotChangeVersion) MutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCurrentSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) SnapshotNotSignedByAllParties <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure . ChangeInputHeadDatum $ replaceParties mutatedParties healthyCurrentOpenDatum
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        n <- choose (1, length healthyOnChainParties - 1)
        fn <- elements [drop n, take n]
        let mutatedParties = fn healthyOnChainParties
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId otherHeadId) headTxOut
    , -- Transaction is signed by a participant
      SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode NoSigners) MutateNoRequiredSigner <$> do
        pure $ ChangeRequiredSigners []
    , SomeMutation (pure $ toErrorCode TooManySigners) MutateMultipleRequiredSigner <$> do
        otherSigners <- listOf1 (genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey))
        let signerAndOthers = somePartyCardanoVerificationKey : otherSigners
        pure $ ChangeRequiredSigners (verificationKeyHash <$> signerAndOthers)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateCloseUTxOHash . ChangeOutput 0 <$> do
        mutatedUTxOHash <- genHash `suchThat` ((/= toBuiltin (hashUTxO @Tx healthySplitUTxOInHead)) . toBuiltin)
        pure $ modifyInlineDatum (replaceUTxOHash $ toBuiltin mutatedUTxOHash) headTxOut
    , -- Correct contestation deadline is set
      SomeMutation (pure $ toErrorCode IncorrectClosedContestationDeadline) MutateContestationDeadline <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateContestationPeriod <$> do
        mutatedPeriod <- arbitrary
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationPeriod mutatedPeriod) headTxOut
    , -- Transaction validity range is bounded
      SomeMutation (pure $ toErrorCode InfiniteLowerBound) MutateInfiniteLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    , -- Transaction validity range is bounded
      SomeMutation (pure $ toErrorCode InfiniteUpperBound) MutateInfiniteUpperBound . ChangeValidityUpperBound <$> do
        pure TxValidityNoUpperBound
    , SomeMutation (pure $ toErrorCode HasBoundedValidityCheckFailed) MutateValidityInterval <$> do
        (lowerSlotNo, upperSlotNo, adjustedContestationDeadline) <- genOversizedTransactionValidity
        pure $
          Changes
            [ ChangeValidityInterval (TxValidityLowerBound lowerSlotNo, TxValidityUpperBound upperSlotNo)
            , ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline adjustedContestationDeadline) headTxOut
            ]
    , -- Transaction is signed by a participant
      -- This is a bit confusing and not giving much value. Maybe we can remove this.
      SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) CloseFromDifferentHead <$> do
        otherHeadId <- headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId headTxOut)
            , ChangeInput
                healthyOpenHeadTxIn
                (replacePolicyIdWith Fixture.testPolicyId otherHeadId $ healthyOpenHeadTxOut datum)
                ( Just $
                    toScriptData
                      ( Head.Close
                          Head.CloseUnusedDec
                            { signature = toPlutusSignatures $ healthySignature healthyCurrentSnapshot
                            }
                      )
                )
            ]
    , -- No minting or burning
      SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , -- Initializes the set of contesters
      SomeMutation (pure $ toErrorCode ContestersNonEmpty) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- resize 3 . listOf1 $ PubKeyHash . toBuiltin <$> genHash
        pure $ headTxOut & modifyInlineDatum (replaceContesters mutatedContesters)
    , -- Value in the head is preserved
      SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateCloseUTxOToDecommitHash . ChangeOutput 0 <$> do
        mutatedHash <- arbitrary `suchThat` (/= (toBuiltin $ hashUTxO @Tx healthySplitUTxOToDecommit))
        pure $ headTxOut & modifyInlineDatum (replaceOmegaUTxOHash mutatedHash)
    ]
 where
  genOversizedTransactionValidity = do
    -- Implicit hypotheses: the slot length is and has always been 1 seconds so we can add slot with seconds
    lowerValidityBound <- arbitrary :: Gen Word64
    upperValidityBound <- choose (lowerValidityBound + fromIntegral healthyContestationPeriodSeconds, maxBound)
    let adjustedContestationDeadline =
          fromMilliSeconds . DiffMilliSeconds $ (healthyContestationPeriodSeconds + fromIntegral upperValidityBound) * 1000
    pure (SlotNo lowerValidityBound, SlotNo upperValidityBound, adjustedContestationDeadline)

  headTxOut = fromJust $ txOuts' tx !!? 0

  datum :: TxOutDatum CtxUTxO
  datum = mkTxOutDatumInline healthyCurrentOpenDatum

-- | Generate not acceptable, but interesting deadlines.
genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline = do
  oneof
    [ valuesAroundZero
    , valuesAroundDeadline
    ]
 where
  valuesAroundZero = arbitrary `suchThat` (/= deadline)

  valuesAroundDeadline = arbitrary `suchThat` (/= 0) <&> (+ deadline)

  deadline = posixFromUTCTime healthyContestationDeadline
