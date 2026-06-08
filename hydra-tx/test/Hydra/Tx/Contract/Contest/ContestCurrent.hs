{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Contest.ContestCurrent where

import Hydra.Cardano.Api
import Hydra.Plutus.Gen ()
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)

import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Contract.Deposit (DepositRedeemer (Claim))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import Hydra.Data.Party (partyFromVerificationKeyBytes)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (mkHeadId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyClosedHeadTxIn,
  healthyClosedHeadTxOut,
  healthyClosedState,
  healthyContestSnapshot,
  healthyContestSnapshotNumber,
  healthyContestationDeadline,
  healthyContesterVerificationKey,
  healthyOnChainContestationPeriod,
  healthyOnChainParties,
  healthyParticipants,
  healthyParties,
  healthySignature,
  healthySlotNo,
 )
import Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Tx.Deposit (mkDepositOutput)
import Hydra.Tx.Snapshot (Snapshot (..))
import Hydra.Tx.Utils (adaOnly)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusLedgerApi.V3 qualified as Plutus
import Test.Gen.Cardano.Api.Typed (genTxValidityLowerBound)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (
  genAddressInEra,
  genHash,
  genMintedOrBurnedValue,
  genUTxOSized,
  genValue,
  genVerificationKey,
 )
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replaceAccumulatorCommitment,
  replaceContestationDeadline,
  replaceContestationPeriod,
  replaceContesters,
  replaceHeadAdaOverhead,
  replaceHeadId,
  replaceParties,
  replacePolicyIdWith,
  replaceSnapshotNumber,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, listOf, listOf1, oneof, resize, suchThat, vectorOf)
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances ()

healthyContestAccumulatorHash :: Head.Hash
healthyContestAccumulatorHash =
  toBuiltin $ Accumulator.getAccumulatorHash $ accumulator healthyContestSnapshot

-- FIXME: Should try to mutate the 'closedAt' recorded time to something else
data ContestMutation
  = -- | Ensures the contest transaction's continuing output is paid to νHead.
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
  | -- | Inject an unrelated v_deposit input into a healthy Contest.
    ContestAbsorbForeignDeposit
  | -- | Invalidates the tx by writing a wrong accumulator commitment in the
    -- output datum while keeping a valid signed accumulatorHash in the redeemer.
    --
    -- Ensures the on-chain validator binds the G2 commitment to the signed hash.
    MutateAccumulatorCommitment
  | -- | Changing headAdaOverhead in the output ClosedDatum must be rejected.
    MutateHeadAdaOverhead
  | -- | Adding extra ADA to the head output must be rejected.
    --
    -- Ensures a malicious contester cannot inflate the head value, which would
    -- cause the strict-equality fanout check to fail (stuck head).
    ContestIncreaseHeadValue
  deriving stock (Generic, Show, Enum, Bounded)

genContestMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (pure $ toErrorCode FailedContestUnused) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
        pure $
          Head.Contest
            Head.ContestUnused
              { signature = toPlutusSignatures mutatedSignature
              , accumulatorHash = healthyContestAccumulatorHash
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
                  Head.ContestUnused
                    { signature =
                        toPlutusSignatures $
                          healthySignature (fromInteger mutatedSnapshotNumber)
                    , accumulatorHash = healthyContestAccumulatorHash
                    }
            ]
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= healthyContesterVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , -- REVIEW: This is a bit confusing and not giving much value. Maybe we can remove this.
      -- This also seems to be covered by MutateRequiredSigner
      SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) ContestFromDifferentHead <$> do
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
                          Head.ContestUnused
                            { signature =
                                toPlutusSignatures $
                                  healthySignature healthyContestSnapshotNumber
                            , accumulatorHash = healthyContestAccumulatorHash
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
    , SomeMutation (pure $ toErrorCode HeadRedeemerNotIncrement) ContestAbsorbForeignDeposit <$> do
        extraIn <- genTxIn
        extraDeposited <- UTxO.map adaOnly <$> genUTxOSized 1
        let
          -- Past the tx upper bound, so the deadline check passes and
          -- the later guard fires instead.
          extraDeadline =
            addUTCTime (60 * 60 * 24) $
              slotNoToUTCTime systemStart slotLength healthySlotNo
          extraDepositOut :: TxOut CtxUTxO
          extraDepositOut =
            mkDepositOutput
              testNetworkId
              (mkHeadId testPolicyId)
              extraDeposited
              extraDeadline
          -- Absorb the deposit's value to keep the tx balanced.
          headTxOut' =
            modifyTxOutValue (<> txOutValue extraDepositOut) headTxOut
        pure $
          Changes
            [ AddInput extraIn extraDepositOut (Just $ toScriptData Claim)
            , ChangeOutput 0 headTxOut'
            , AddScript depositValidatorScript
            ]
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) MutateAccumulatorCommitment . ChangeOutput 0 <$> do
        -- A commitment from a different accumulator: the signed accumulatorHash
        -- was derived from the healthy one, so this G2 point won't match.
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode ChangedHeadAdaOverhead) MutateHeadAdaOverhead . ChangeOutput 0 <$> do
        wrongOverhead <- arbitrary `suchThat` (/= 0)
        pure $ headTxOut & modifyInlineDatum (replaceHeadAdaOverhead wrongOverhead)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) ContestIncreaseHeadValue <$> do
        extraLovelace <- lovelaceToValue . Coin <$> choose (1, 10_000_000)
        pure $ ChangeOutput 0 (modifyTxOutValue (<> extraLovelace) headTxOut)
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  slotOverContestationDeadline slotNo =
    slotNoToUTCTime systemStart slotLength slotNo > healthyContestationDeadline
