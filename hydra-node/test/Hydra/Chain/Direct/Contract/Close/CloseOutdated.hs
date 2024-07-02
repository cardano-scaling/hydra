{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close.CloseOutdated where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Gen (genHash, genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replaceContestationDeadline,
  replaceContestationPeriod,
  replaceContesters,
  replaceHeadId,
  replaceParties,
  replacePolicyIdWith,
  replaceSnapshotNumber,
  replaceUtxoHash,
 )
import Hydra.Chain.Direct.Contract.Close.Healthy (healthyContestationDeadline, healthyContestationPeriodSeconds, healthyOnChainParties, healthyOpenDatum, healthyOpenHeadTxIn, healthyOpenHeadTxOut, healthySignature, somePartyCardanoVerificationKey, healthySplitUTxOInHead, healthySplitUTxOToDecommit, healthyCloseUTxOHash, healthyConfirmedClosingSnapshot, healthyConfirmedClosingSnapshotTx)
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.Tx (ClosingSnapshot (..), UTxOHash (UTxOHash), mkHeadId)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import Hydra.Crypto (MultiSignature (..), toPlutusSignatures)
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genAddressInEra, genValue, genVerificationKey)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), fromMilliSeconds)
import PlutusLedgerApi.V2 (POSIXTime, PubKeyHash (PubKeyHash), toBuiltin)
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, listOf1, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyOutdatedSnapshotNumber :: SnapshotNumber
healthyOutdatedSnapshotNumber = 1

healthyOutdatedSnapshotVersion :: SnapshotVersion
healthyOutdatedSnapshotVersion = 1

--- XXX: Decommit snapshot which we want to mimick so that we test how close
--- behaves after decommit.
healthyOutdatedSnapshot :: Snapshot Tx
healthyOutdatedSnapshot =
  Snapshot
    { headId = mkHeadId Fixture.testPolicyId
    , number = healthyOutdatedSnapshotNumber
    , utxo = healthySplitUTxOInHead
    , confirmed = []
    , -- XXX even after observing a decrement tx,
      -- the snapshot still contains something to decommit.
      utxoToDecommit = Just healthySplitUTxOToDecommit
    , version = healthyOutdatedSnapshotVersion
    }

healthyOutdatedOpenDatum :: Head.State
healthyOutdatedOpenDatum = healthyOpenDatum healthyOutdatedSnapshot

healthyOutdatedConfirmedClosingSnapshot :: ClosingSnapshot
healthyOutdatedConfirmedClosingSnapshot = healthyConfirmedClosingSnapshot healthyOutdatedSnapshot

healthyCloseOutdatedTx :: (Tx, UTxO)
healthyCloseOutdatedTx = healthyConfirmedClosingSnapshotTx healthyOutdatedSnapshot


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
  | -- TODO: | -- Check the snapshot version is preserved from last open state.
    --   MutateSnapshotVersion

    -- | Check that snapshot numbers = 0 need to close the head with the
    -- initial UTxO hash.
    MutateInitialSnapshotNumber
  | -- | Ensures the close snapshot is multisigned by all Head participants by
    -- changing the parties in the input head datum. If they do not align the
    -- multisignature will not be valid anymore.
    SnapshotNotSignedByAllParties
  | -- | Ensures close is authenticated by a one of the Head members by changing
    --  the signer used on the tx to not be one of PTs.
    MutateRequiredSigner
  | -- | Ensures close is authenticated by a one of the Head members by changing
    --  the signer used on the tx to be empty.
    MutateNoRequiredSigner
  | -- | Ensures close is authenticated by a one of the Head members by changing
    --  the signer used on the tx to have multiple signers (including the signer
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
  | -- | Invalidates the tx by changing the version in redeemer.
    --
    -- Ensures the output state is consistent with the redeemer.
    MutateCloseVersion
  | -- | Invalidates the tx by changing the signatures in redeemer.
    --
    -- Ensures the output state is consistent with the redeemer.
    MutateCloseSignatures
  | -- | Ensures parties do not change between head input datum and head output
    --  datum.
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

genCloseOutdatedMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseOutdatedMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        sigs <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $ Head.Close sigs Head.CurrentVersion mempty
    , SomeMutation (pure $ toErrorCode ClosedWithNonInitialHash) MutateInitialSnapshotNumber <$> do
        let mutatedSnapshotNumber = 0
        pure $
          Changes
            [ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber mutatedSnapshotNumber) headTxOut
            , ChangeInputHeadDatum healthyOutdatedOpenDatum{Head.utxoHash = ""}
            ]
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (> healthyOutdatedSnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , -- , -- XXX: Last known open state version is recorded in closed state
      --   SomeMutation (pure $ toErrorCode LastKnownVersionIsNotMatching) MutateSnapshotVersion <$> do
      --     mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (> healthyCloseSnapshotVersion)
      --     pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersionInClosed $ toInteger mutatedSnapshotVersion) headTxOut
      SomeMutation (pure $ toErrorCode SignatureVerificationFailed) SnapshotNotSignedByAllParties . ChangeInputHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ healthyOutdatedOpenDatum{Head.parties = mutatedParties}
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        n <- choose (1, length healthyOnChainParties - 1)
        fn <- elements [drop n, take n]
        let mutatedParties = fn healthyOnChainParties
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId otherHeadId) headTxOut
    , -- XXX: Transaction is signed by a participant
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
        mutatedUTxOHash <- genHash `suchThat` ((/= healthyCloseUTxOHash) . toBuiltin)
        pure $ modifyInlineDatum (replaceUtxoHash $ toBuiltin mutatedUTxOHash) headTxOut
    , -- XXX: Correct contestation deadline is set
      SomeMutation (pure $ toErrorCode IncorrectClosedContestationDeadline) MutateContestationDeadline <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateContestationPeriod <$> do
        mutatedPeriod <- arbitrary
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationPeriod mutatedPeriod) headTxOut
    , -- XXX: Transaction validity range is bounded
      SomeMutation (pure $ toErrorCode InfiniteLowerBound) MutateInfiniteLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    , -- XXX: Transaction validity range is bounded
      SomeMutation (pure $ toErrorCode InfiniteUpperBound) MutateInfiniteUpperBound . ChangeValidityUpperBound <$> do
        pure TxValidityNoUpperBound
    , SomeMutation (pure $ toErrorCode HasBoundedValidityCheckFailed) MutateValidityInterval <$> do
        (lowerSlotNo, upperSlotNo, adjustedContestationDeadline) <- genOversizedTransactionValidity
        pure $
          Changes
            [ ChangeValidityInterval (TxValidityLowerBound lowerSlotNo, TxValidityUpperBound upperSlotNo)
            , ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline adjustedContestationDeadline) headTxOut
            ]
    , -- XXX: Transaction is signed by a participant
      -- This is a bit confusing and not giving much value. Maybe we can remove this.
      SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) CloseFromDifferentHead <$> do
        otherHeadId <- headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        let expectedHash = toBuiltin $ hashUTxO @Tx (fromMaybe mempty $ utxoToDecommit healthyOutdatedSnapshot)
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId headTxOut)
            , ChangeInput
                healthyOpenHeadTxIn
                (replacePolicyIdWith Fixture.testPolicyId otherHeadId $ healthyOpenHeadTxOut datum)
                ( Just $
                    toScriptData
                      ( Head.Close
                          { signature =
                              toPlutusSignatures $
                                healthySignature healthyOutdatedSnapshot
                          , version = Head.CurrentVersion
                          , utxoToDecommitHash = expectedHash
                          }
                      )
                )
            ]
    , -- XXX: No minting or burning
      SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , -- XXX: Initializes the set of contesters
      SomeMutation (pure $ toErrorCode ContestersNonEmpty) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- listOf1 $ PubKeyHash . toBuiltin <$> genHash
        pure $ headTxOut & modifyInlineDatum (replaceContesters mutatedContesters)
    , -- XXX: Value in the head is preserved
      SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateCloseUTxOToDecommitHash . ChangeHeadRedeemer <$> do
        let UTxOHash expectedHash = closeUtxoToDecommitHash healthyOutdatedConfirmedClosingSnapshot
        -- XXX: Close redeemer contains the hash of a decommit utxo. If we
        -- change it should cause invalid signature error.
        pure $ Head.Close (toPlutusSignatures $ signatures healthyOutdatedConfirmedClosingSnapshot) Head.OutdatedVersion (toBuiltin $ expectedHash <> "0")
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateCloseSignatures . ChangeHeadRedeemer <$> do
        let UTxOHash expectedHash = closeUtxoToDecommitHash healthyOutdatedConfirmedClosingSnapshot
        sigs <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        -- XXX: Close redeemer contains the signatures. If we
        -- change them should cause invalid signature error.
        pure $ Head.Close sigs Head.OutdatedVersion (toBuiltin expectedHash)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateCloseVersion . ChangeHeadRedeemer <$> do
        let UTxOHash expectedHash = closeUtxoToDecommitHash healthyOutdatedConfirmedClosingSnapshot
        -- XXX: Close redeemer contains the appropriate version. If we
        -- change it then it should cause invalid signature error.
        pure $ Head.Close (toPlutusSignatures $ signatures healthyOutdatedConfirmedClosingSnapshot) Head.CurrentVersion (toBuiltin expectedHash)
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

  datum = toUTxOContext (mkTxOutDatumInline healthyOutdatedOpenDatum)

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
