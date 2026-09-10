{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestDec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx (registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyClosedHeadTxIn,
  healthyClosedSnapshotNumber,
  healthyContestSnapshot,
  healthyContestSnapshotNumber,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyContesterVerificationKey,
  healthyOnChainContestationPeriod,
  healthyOnChainParties,
  healthyParticipants,
  healthySignature,
  healthySlotNo,
 )
import Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotVersion, commitOutputsHash)
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (toBuiltin)
import Test.Hydra.Tx.Fixture (dperiod, slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genScriptRegistry)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceAccumulatorCommitment,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyContestAccumulatorHash :: Head.Hash
healthyContestAccumulatorHash =
  toBuiltin $ Accumulator.getAccumulatorHash $ accumulator healthyContestSnapshot

healthyContestAppliedAccumulatorHash :: Head.Hash
healthyContestAppliedAccumulatorHash =
  toBuiltin $ Accumulator.getAccumulatorHash $ appliedAccumulator healthyContestSnapshot

healthyContestDecommitOutputsHash :: Head.Hash
healthyContestDecommitOutputsHash =
  toBuiltin $ hashUTxO @Tx (fromMaybe mempty (utxoToDecommit healthyContestSnapshot))

healthyContestCommitOutputsHash :: Head.Hash
healthyContestCommitOutputsHash =
  toBuiltin $ commitOutputsHash healthyContestSnapshot

-- * ContestUsed: the decommit was paid out before the close

-- | The head was closed after the DecrementTx of 'healthyContestSnapshot' had
-- happened, so the open (and closed) version is one ahead of the snapshot's.
healthyContestUsedOpenVersion :: SnapshotVersion
healthyContestUsedOpenVersion = healthyCloseSnapshotVersion + 1

-- | Closed head at 'healthyContestUsedOpenVersion' with an older snapshot.
healthyClosedUsedState :: Head.State
healthyClosedUsedState =
  Head.Closed
    Head.ClosedDatum
      { snapshotNumber = fromIntegral healthyClosedSnapshotNumber
      , parties = healthyOnChainParties
      , contestationDeadline = posixFromUTCTime healthyContestationDeadline
      , contestationPeriod = healthyOnChainContestationPeriod
      , depositPeriod = DP.toChain dperiod
      , headId = toPlutusCurrencySymbol testPolicyId
      , contesters = []
      , version = toInteger healthyContestUsedOpenVersion
      , accumulatorCommitment = Accumulator.getAccumulatorCommitment (appliedAccumulator healthyContestSnapshot)
      , headAdaOverhead = 0
      }

healthyClosedUsedHeadTxOut :: TxOut CtxUTxO
healthyClosedUsedHeadTxOut =
  mkHeadOutput
    testNetworkId
    testPolicyId
    (verificationKeyToOnChainId <$> healthyParticipants)
    (mkTxOutDatumInline healthyClosedUsedState)

-- | Healthy contest with the ContestUsed redeemer: the contesting snapshot is
-- one version behind the head because its decommit was already paid out, so the
-- head holds 'utxo' only and the stored commitment must be the snapshot's
-- 'appliedAccumulator'.
healthyContestUsedTx :: (Tx, UTxO)
healthyContestUsedTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton healthyClosedHeadTxIn healthyClosedUsedHeadTxOut
      <> registryUTxO scriptRegistry

  tx =
    contestTx
      scriptRegistry
      healthyContesterVerificationKey
      (mkHeadId testPolicyId)
      healthyContestationPeriod
      healthyContestUsedOpenVersion
      healthyContestSnapshot
      (healthySignature healthyContestSnapshotNumber)
      (healthySlotNo, slotNoToUTCTime systemStart slotLength healthySlotNo)
      closedThreadOutput

  scriptRegistry = genScriptRegistry `generateWith` 42

  closedThreadOutput =
    ClosedThreadOutput
      { closedThreadUTxO = (healthyClosedHeadTxIn, healthyClosedUsedHeadTxOut)
      , closedParties = healthyOnChainParties
      , closedContestationDeadline = posixFromUTCTime healthyContestationDeadline
      , closedContesters = []
      , closedHeadAdaOverhead = 0
      , closedDepositPeriod = DP.toChain dperiod
      }

data ContestUsedMutation
  = -- | A wrong signature must be rejected by the ContestUsed signature check.
    ContestUsedAlterSignature
  | -- | Stores the snapshot's own accumulator, which still counts the decommit
    -- the decrement already paid out. Both hashes are signed, so only the
    -- redeemer-kind selection rejects this (GHSA-f825-9gwc-h5xq).
    ContestUsedStoreSnapshotAccumulator
  | ContestUsedAlterAccumulatorCommitment
  | -- | Claiming ContestUnused for a snapshot one version behind fails the
    -- signature check at the current version.
    ContestUsedClaimUnused
  deriving stock (Generic, Show, Enum, Bounded)

genContestUsedMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestUsedMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode FailedContestUsed) ContestUsedAlterSignature . ChangeHeadRedeemer <$> do
        mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
        pure $
          Head.Contest
            Head.ContestUsed
              { signature = toPlutusSignatures mutatedSignature
              , accumulatorHash = healthyContestAccumulatorHash
              , appliedAccumulatorHash = healthyContestAppliedAccumulatorHash
              , decommitOutputsHash = healthyContestDecommitOutputsHash
              , commitOutputsHash = healthyContestCommitOutputsHash
              }
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUsedStoreSnapshotAccumulator . ChangeOutput 0 <$> do
        let snapshotCommitment = Accumulator.getAccumulatorCommitment (accumulator healthyContestSnapshot)
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment snapshotCommitment)
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUsedAlterAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode FailedContestUnused) ContestUsedClaimUnused . ChangeHeadRedeemer <$> do
        pure $
          Head.Contest
            Head.ContestUnused
              { signature = toPlutusSignatures $ healthySignature healthyContestSnapshotNumber
              , accumulatorHash = healthyContestAccumulatorHash
              , appliedAccumulatorHash = healthyContestAppliedAccumulatorHash
              , decommitOutputsHash = healthyContestDecommitOutputsHash
              , commitOutputsHash = healthyContestCommitOutputsHash
              }
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

data ContestDecMutation
  = ContestUnusedDecAlterRedeemerDecommitHash
  | ContestUsedDecAlterAccumulatorCommitment
  | ContestUnusedDecAlterAccumulatorCommitment
  | -- | Stores the applied accumulator, which excludes the pending decommit the
    -- head still holds. Both hashes are signed, so only the redeemer-kind
    -- selection rejects this (GHSA-f825-9gwc-h5xq).
    ContestUnusedDecStoreAppliedAccumulator
  | ContestUsedDecMutateSnapshotVersion
  | ContestUnusedDecMutateSnapshotVersion
  deriving stock (Generic, Show, Enum, Bounded)

genContestDecMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestDecMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ContestUnusedDecAlterRedeemerDecommitHash . ChangeHeadRedeemer <$> do
        mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
        pure $
          Head.Contest
            Head.ContestUnused
              { signature = toPlutusSignatures mutatedSignature
              , accumulatorHash = healthyContestAccumulatorHash
              , appliedAccumulatorHash = healthyContestAppliedAccumulatorHash
              , decommitOutputsHash = healthyContestDecommitOutputsHash
              , commitOutputsHash = healthyContestCommitOutputsHash
              }
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUsedDecAlterAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUnusedDecAlterAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUnusedDecStoreAppliedAccumulator . ChangeOutput 0 <$> do
        let appliedCommitment = Accumulator.getAccumulatorCommitment (appliedAccumulator healthyContestSnapshot)
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment appliedCommitment)
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUsedDecMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUnusedDecMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
