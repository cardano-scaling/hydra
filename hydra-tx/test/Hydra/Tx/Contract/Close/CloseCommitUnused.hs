{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.CloseCommitUnused where

import Hydra.Cardano.Api
import Hydra.Plutus.Gen ()
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.UtilError (UtilError (MintingOrBurningIsForbidden))
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (Snapshot (..), mkHeadId, registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import Hydra.Tx.Contract.Close.Healthy (
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
  somePartyCardanoVerificationKey,
 )
import Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Tx.Snapshot (getSnapshot)
import Hydra.Tx.Snapshot qualified as Snapshot
import Hydra.Tx.Utils (IncrementalAction (..), setIncrementalActionMaybe)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), fromMilliSeconds)
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash (PubKeyHash), toBuiltin)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (
  genAddressInEra,
  genHash,
  genMintedOrBurnedValue,
  genScriptRegistry,
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
  replaceHeadId,
  replaceParties,
  replacePolicyIdWith,
  replaceSnapshotNumber,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, listOf1, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()

-- | UTxO being committed (deposit not yet merged into the head).
healthyDepositUTxO :: UTxO
healthyDepositUTxO = genUTxOSized 3 `generateWith` 42

healthyCommitPendingSnapshotNumber :: Snapshot.SnapshotNumber
healthyCommitPendingSnapshotNumber = 1

healthyCommitPendingSnapshotVersion :: Snapshot.SnapshotVersion
healthyCommitPendingSnapshotVersion = 1

-- | Snapshot with a pending commit: version matches the open state version.
-- Exercises the (ToCommit, True) branch of headAdaOverhead computation in closeTx.
healthyCommitPendingSnapshot :: Snapshot Tx
healthyCommitPendingSnapshot =
  Snapshot
    { headId = mkHeadId Fixture.testPolicyId
    , version = healthyCommitPendingSnapshotVersion
    , number = healthyCommitPendingSnapshotNumber
    , confirmed = []
    , utxo = healthySplitUTxOInHead
    , utxoToCommit = Just healthyDepositUTxO
    , utxoToDecommit = Nothing
    , accumulator = Accumulator.buildFromSnapshotUTxOs healthySplitUTxOInHead (Just healthyDepositUTxO) Nothing
    }

healthyCommitPendingAccumulatorHash :: Head.Hash
healthyCommitPendingAccumulatorHash =
  toBuiltin $ Accumulator.getAccumulatorHash $ accumulator healthyCommitPendingSnapshot

healthyCommitPendingOpenDatum :: Head.State
healthyCommitPendingOpenDatum =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , contestationPeriod = healthyContestationPeriod
      , headSeed = toPlutusTxOutRef Fixture.testSeedInput
      , headId = toPlutusCurrencySymbol Fixture.testPolicyId
      , version = toInteger healthyCommitPendingSnapshotVersion
      , accumulatorHash = healthyCommitPendingAccumulatorHash
      }

-- | Healthy close transaction for the case of closing with a pending (unused) commit.
-- The snapshot version equals the open state version, so the deposit has not yet been
-- merged into the head — the CloseUnused redeemer is used.
healthyCloseCommitPendingTx :: (Tx, UTxO)
healthyCloseCommitPendingTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId Fixture.testPolicyId)
      healthyCommitPendingSnapshotVersion
      commitPendingSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      incrementalAction

  commitPendingSnapshot = healthyConfirmedSnapshot healthyCommitPendingSnapshot

  incrementalAction =
    fromMaybe NoThing $
      setIncrementalActionMaybe
        (utxoToCommit $ getSnapshot commitPendingSnapshot)
        (utxoToDecommit $ getSnapshot commitPendingSnapshot)

  lookupUTxO :: UTxO
  lookupUTxO =
    UTxO.singleton healthyOpenHeadTxIn (healthyOpenHeadTxOut datum)
      <> registryUTxO scriptRegistry

  scriptRegistry = genScriptRegistry `generateWith` 42

  datum :: TxOutDatum CtxUTxO
  datum = mkTxOutDatumInline healthyCommitPendingOpenDatum

  openThreadOutput :: OpenThreadOutput
  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut datum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }

data CloseMutation
  = NotContinueContract
  | MutateSignatureButNotSnapshotNumber
  | MutateSnapshotNumberButNotSignature
  | MutateSnapshotVersion
  | SnapshotNotSignedByAllParties
  | MutateRequiredSigner
  | MutateNoRequiredSigner
  | MutateMultipleRequiredSigner
  | MutatePartiesInOutput
  | MutateHeadIdInOutput
  | MutateInfiniteLowerBound
  | MutateInfiniteUpperBound
  | MutateContestationDeadline
  | MutateValidityInterval
  | CloseFromDifferentHead
  | MutateTokenMintingOrBurning
  | MutateContesters
  | MutateValueInOutput
  | MutateContestationPeriod
  | MutateAccumulatorCommitment
  deriving stock (Generic, Show, Enum, Bounded)

genCloseCommitUnusedMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseCommitUnusedMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        signature <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $ Head.Close Head.CloseUnused{signature, accumulatorHash = healthyCommitPendingAccumulatorHash}
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (> healthyCommitPendingSnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) MutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCommitPendingSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) SnapshotNotSignedByAllParties <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure . ChangeInputHeadDatum $ replaceParties mutatedParties healthyCommitPendingOpenDatum
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        n <- choose (1, length healthyOnChainParties - 1)
        fn <- elements [drop n, take n]
        let mutatedParties = fn healthyOnChainParties
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId otherHeadId) headTxOut
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode NoSigners) MutateNoRequiredSigner <$> do
        pure $ ChangeRequiredSigners []
    , SomeMutation (pure $ toErrorCode TooManySigners) MutateMultipleRequiredSigner <$> do
        otherSigners <- listOf1 (genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey))
        let signerAndOthers = somePartyCardanoVerificationKey : otherSigners
        pure $ ChangeRequiredSigners (verificationKeyHash <$> signerAndOthers)
    , SomeMutation (pure $ toErrorCode IncorrectClosedContestationDeadline) MutateContestationDeadline <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateContestationPeriod <$> do
        mutatedPeriod <- arbitrary
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationPeriod mutatedPeriod) headTxOut
    , SomeMutation (pure $ toErrorCode InfiniteLowerBound) MutateInfiniteLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    , SomeMutation (pure $ toErrorCode InfiniteUpperBound) MutateInfiniteUpperBound . ChangeValidityUpperBound <$> do
        pure TxValidityNoUpperBound
    , SomeMutation (pure $ toErrorCode HasBoundedValidityCheckFailed) MutateValidityInterval <$> do
        (lowerSlotNo, upperSlotNo, adjustedContestationDeadline) <- genOversizedTransactionValidity
        pure $
          Changes
            [ ChangeValidityInterval (TxValidityLowerBound lowerSlotNo, TxValidityUpperBound upperSlotNo)
            , ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline adjustedContestationDeadline) headTxOut
            ]
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) CloseFromDifferentHead <$> do
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
                          Head.CloseUnused
                            { signature =
                                toPlutusSignatures $
                                  healthySignature healthyCommitPendingSnapshot
                            , accumulatorHash = healthyCommitPendingAccumulatorHash
                            }
                      )
                )
            ]
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (pure $ toErrorCode ContestersNonEmpty) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- resize 3 . listOf1 $ PubKeyHash . toBuiltin <$> genHash
        pure $ headTxOut & modifyInlineDatum (replaceContesters mutatedContesters)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) MutateAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    ]
 where
  genOversizedTransactionValidity = do
    lowerValidityBound <- arbitrary :: Gen Word64
    upperValidityBound <- choose (lowerValidityBound + fromIntegral healthyContestationPeriodSeconds, maxBound)
    let adjustedContestationDeadline =
          fromMilliSeconds . DiffMilliSeconds $ (healthyContestationPeriodSeconds + fromIntegral upperValidityBound) * 1000
    pure (SlotNo lowerValidityBound, SlotNo upperValidityBound, adjustedContestationDeadline)

  headTxOut = fromJust $ txOuts' tx !!? 0

  datum :: TxOutDatum CtxUTxO
  datum = mkTxOutDatumInline healthyCommitPendingOpenDatum

genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline =
  oneof
    [ valuesAroundZero
    , valuesAroundDeadline
    ]
 where
  valuesAroundZero = arbitrary `suchThat` (/= deadline)
  valuesAroundDeadline = arbitrary `suchThat` (/= 0) <&> (+ deadline)
  deadline = posixFromUTCTime healthyContestationDeadline
