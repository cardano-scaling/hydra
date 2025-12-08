{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Contest.ContestCurrent where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import Hydra.Data.Party (partyFromVerificationKeyBytes)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx.Contract.Commit (genMintedOrBurnedValue)
import Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyClosedHeadTxIn,
  healthyClosedHeadTxOut,
  healthyClosedState,
  healthyContestSnapshotNumber,
  healthyContestUTxOHash,
  healthyContestUTxOToDecommitHash,
  healthyContestationDeadline,
  healthyContesterVerificationKey,
  healthyOnChainContestationPeriod,
  healthyOnChainParties,
  healthyParticipants,
  healthyParties,
  healthySignature,
 )
import Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Tx.Snapshot (Snapshot (..))
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusLedgerApi.V3 qualified as Plutus
import Test.Gen.Cardano.Api.Typed (genTxValidityLowerBound)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (
  genAddressInEra,
  genHash,
  genValue,
  genVerificationKey,
 )
import Test.Hydra.Tx.Mutation (
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
import Test.QuickCheck (arbitrarySizedNatural, listOf, listOf1, oneof, resize, suchThat, vectorOf)
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances ()

-- FIXME: Should try to mutate the 'closedAt' recorded time to something else
data ContestMutation
  = -- | Ensures collectCom does not allow any output address but νHead.
    NotContinueContract
  | -- | Invalidates the tx by changing the redeemer signature but not the
    -- snapshot number in resulting head output.
    --
    -- Ensures the snapshot signature is multisigned by all valid Head
    -- participants.
    MutateSignatureButNotSnapshotNumber
  | -- | Invalidates the tx by changing the snapshot number in resulting head
    -- output but not the redeemer signature.
    --
    -- Ensures the snapshot signature is aligned with snapshot number.
    MutateSnapshotNumberButNotSignature
  | -- | Check the snapshot version is preserved from last open state.
    MutateSnapshotVersion
  | -- | Invalidates the tx by changing the contest snapshot number too old.
    --
    -- This is achieved by updating the head input datum to be older, so the
    -- healthy snapshot number becomes too old.
    MutateToNonNewerSnapshot
  | -- | Ensures close is authenticated by one of the Head members by changing the signer
    -- used on the tx to be not one of PTs.
    MutateRequiredSigner
  | -- | Ensures close is authenticated by one of the Head members by changing the signer
    -- used on the tx to be empty.
    MutateNoRequiredSigner
  | -- | Ensures close is authenticated by one of the Head members by changing the signer
    -- used on the tx to have multiple signers (including the signer to not fail for
    -- SignerIsNotAParticipant).
    MutateMultipleRequiredSigner
  | -- | Invalidates the tx by changing the utxo hash in resulting head output.
    --
    -- Ensures the output state is consistent with the redeemer.
    MutateContestUTxOHash
  | -- | Ensures the contest snapshot is multisigned by all Head participants by
    -- changing the parties in the input head datum. If they do not align the
    -- multisignature will not be valid anymore.
    SnapshotNotSignedByAllParties
  | -- | Invalidates the tx by changing the upper bound to be beyond
    -- contestation deadline from head input (stored state).
    MutateValidityPastDeadline
  | -- | Change the head policy id to simulate contestation using a ST and
    -- signer from a different head. The signer shows a correct signature but
    -- from a different head. This will cause the signer to not be present in
    -- the participation tokens.
    ContestFromDifferentHead
  | -- | Minting or burning of tokens should not be possible in contest.
    MutateTokenMintingOrBurning
  | -- | Ensures a participant can only contest once by changing the head input
    -- datum to already include the signer.
    MutateInputContesters
  | -- | Ensures a the signer needs to be added to the head output datum.
    MutateContesters
  | -- | Invalidates the tx by changing the output values arbitrarily to be
    -- different (not preserved) from the head.
    --
    -- Ensures values are preserved between head input and output.
    MutateValueInOutput
  | -- | Not pushing the contestation deadline in head output datum should not
    -- be allowed.
    NotUpdateDeadlineAlthoughItShould
  | -- | Pushes the deadline although this is the last contest. Instead of
    -- creating another healthy case and mutate that one, this mutation just
    -- changes the starting situation so that everyone else already contested.
    -- Remember the 'healthyContestTx' is already pushing out the deadline.
    PushDeadlineAlthoughItShouldNot
  | -- | Ensures contestation period does not change between head input datum
    -- and head output datum.
    MutateOutputContestationPeriod
  | -- | Ensures parties do not change between head input datum and head output
    --  datum.
    MutatePartiesInOutput
  | -- | Ensures headId do not change between head input datum and head output
    -- datum.
    MutateHeadIdInOutput
  deriving stock (Generic, Show, Enum, Bounded)

genContestMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (pure $ toErrorCode FailedContestCurrent) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
        pure $
          Head.Contest
            Head.ContestCurrent
              { signature = toPlutusSignatures mutatedSignature
              }
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (> healthyContestSnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , -- Last known open state version stays recorded in closed state
      SomeMutation (pure $ toErrorCode MustNotChangeVersion) MutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode TooOldSnapshot) MutateToNonNewerSnapshot <$> do
        mutatedSnapshotNumber <- choose (toInteger healthyContestSnapshotNumber, toInteger healthyContestSnapshotNumber + 1)
        pure $
          Changes
            [ ChangeInputHeadDatum $
                healthyClosedState & replaceSnapshotNumber mutatedSnapshotNumber
            , ChangeHeadRedeemer $
                Head.Contest
                  Head.ContestCurrent
                    { signature =
                        toPlutusSignatures $
                          healthySignature (fromInteger mutatedSnapshotNumber)
                    }
            ]
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= healthyContesterVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , -- REVIEW: This is a bit confusing and not giving much value. Maybe we can remove this.
      -- This also seems to be covered by MutateRequiredSigner
      SomeMutation (pure $ toErrorCode FailedContestCurrent) ContestFromDifferentHead <$> do
        otherHeadId <- headPolicyId <$> arbitrary `suchThat` (/= healthyClosedHeadTxIn)
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith testPolicyId otherHeadId headTxOut)
            , ChangeInput
                healthyClosedHeadTxIn
                (replacePolicyIdWith testPolicyId otherHeadId healthyClosedHeadTxOut)
                ( Just $
                    toScriptData
                      ( Head.Contest
                          Head.ContestCurrent
                            { signature =
                                toPlutusSignatures $
                                  healthySignature healthyContestSnapshotNumber
                            }
                      )
                )
            ]
    , SomeMutation (pure $ toErrorCode NoSigners) MutateNoRequiredSigner <$> do
        pure $ ChangeRequiredSigners []
    , SomeMutation (pure $ toErrorCode TooManySigners) MutateMultipleRequiredSigner <$> do
        otherSigners <- listOf1 (genVerificationKey `suchThat` (/= healthyContesterVerificationKey))
        let signerAndOthers = healthyContesterVerificationKey : otherSigners
        pure $ ChangeRequiredSigners (verificationKeyHash <$> signerAndOthers)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateContestUTxOHash . ChangeOutput 0 <$> do
        mutatedUTxOHash <- genHash `suchThat` ((/= healthyContestUTxOHash) . toBuiltin)
        pure $
          modifyInlineDatum
            (replaceUTxOHash (toBuiltin mutatedUTxOHash))
            headTxOut
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateContestUTxOHash . ChangeOutput 0 <$> do
        mutatedUTxOHash <- arbitrary `suchThat` (/= healthyContestUTxOToDecommitHash)
        pure $
          modifyInlineDatum
            (replaceOmegaUTxOHash mutatedUTxOHash)
            headTxOut
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) SnapshotNotSignedByAllParties . ChangeInputHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $
          healthyClosedState & replaceParties mutatedParties
    , SomeMutation (pure $ toErrorCode UpperBoundBeyondContestationDeadline) MutateValidityPastDeadline . ChangeValidityInterval <$> do
        lb <- hedgehog $ genTxValidityLowerBound cardanoEra
        ub <- TxValidityUpperBound <$> arbitrary `suchThat` slotOverContestationDeadline
        pure (lb, ub)
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (pure $ toErrorCode SignerAlreadyContested) MutateInputContesters . ChangeInputHeadDatum <$> do
        let contester = toPlutusKeyHash (verificationKeyHash healthyContesterVerificationKey)
        mutatedContesters <- do
          contesters <- resize (length healthyParticipants - 1) . listOf $ Plutus.PubKeyHash . toBuiltin <$> genHash
          pure (contester : contesters)
        pure $
          healthyClosedState & replaceContesters mutatedContesters
    , SomeMutation (pure $ toErrorCode ContesterNotIncluded) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- resize (length healthyParticipants) . listOf $ Plutus.PubKeyHash . toBuiltin <$> genHash
        pure $ modifyInlineDatum (replaceContesters mutatedContesters) headTxOut
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode MustPushDeadline) NotUpdateDeadlineAlthoughItShould . ChangeOutput 0 <$> do
        let deadline = posixFromUTCTime healthyContestationDeadline
        -- Here we are replacing the contestationDeadline using the previous so we are not _pushing it_ further
        -- Remember the 'healthyContestTx' is already pushing out the deadline.
        pure $ headTxOut & modifyInlineDatum (replaceContestationDeadline deadline)
    , SomeMutation (pure $ toErrorCode MustNotPushDeadline) PushDeadlineAlthoughItShouldNot <$> do
        alreadyContested <- vectorOf (length healthyParties - 1) $ Plutus.PubKeyHash . toBuiltin <$> genHash
        let contester = toPlutusKeyHash $ verificationKeyHash healthyContesterVerificationKey
        pure $
          Changes
            [ ChangeOutput 0 (headTxOut & modifyInlineDatum (replaceContesters (contester : alreadyContested)))
            , ChangeInputHeadDatum (healthyClosedState & replaceContesters alreadyContested)
            ]
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateOutputContestationPeriod <$> do
        randomCP <- arbitrary `suchThat` (/= healthyOnChainContestationPeriod)
        pure $ ChangeOutput 0 (headTxOut & modifyInlineDatum (replaceContestationPeriod randomCP))
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        mutatedParties <-
          -- The length of mutatedParties must be the same as
          -- healthyOnChainParties so to not fail because of
          -- `must not push contestation deadline`.
          vectorOf
            (length healthyOnChainParties)
            (partyFromVerificationKeyBytes <$> genHash)
            `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId otherHeadId) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  slotOverContestationDeadline slotNo =
    slotNoToUTCTime systemStart slotLength slotNo > healthyContestationDeadline
