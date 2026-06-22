{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestInc where

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
import Hydra.Tx (mkHeadId, registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyClosedHeadTxIn,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyContesterVerificationKey,
  healthyOnChainContestationPeriod,
  healthyOnChainParties,
  healthyParticipants,
  healthySigningKeys,
  healthySlotNo,
  splitUTxOInHead,
 )
import Hydra.Tx.Crypto (MultiSignature, aggregate, sign, toPlutusSignatures)
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber)
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (toBuiltin)
import Test.Hydra.Tx.Fixture (dperiod, slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genScriptRegistry, genUTxOSized)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceAccumulatorCommitment,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, oneof, suchThat)
import Test.QuickCheck.Instances ()

-- | UTxO being committed (deposit not yet merged into the head).
healthyDepositedUTxO :: UTxO
healthyDepositedUTxO = genUTxOSized 3 `generateWith` 42

healthyContestIncSnapshotNumber :: SnapshotNumber
healthyContestIncSnapshotNumber = 4

-- | Snapshot with a pending commit, exercising the ToCommit accumulator path.
healthyContestIncSnapshot :: Snapshot Tx
healthyContestIncSnapshot =
  Snapshot
    { headId = mkHeadId testPolicyId
    , number = healthyContestIncSnapshotNumber
    , utxo = splitUTxOInHead
    , confirmed = []
    , utxoToCommit = Just healthyDepositedUTxO
    , utxoToDecommit = Nothing
    , version = healthyCloseSnapshotVersion
    , accumulator = Accumulator.buildFromSnapshotUTxOs splitUTxOInHead (Just healthyDepositedUTxO) Nothing
    }

healthyContestIncAccumulatorHash :: Head.Hash
healthyContestIncAccumulatorHash =
  toBuiltin $ Accumulator.getAccumulatorHash $ accumulator healthyContestIncSnapshot

-- | ClosedDatum whose accumulatorCommitment was derived from a commit-type snapshot.
healthyContestIncClosedState :: Head.State
healthyContestIncClosedState =
  Head.Closed
    Head.ClosedDatum
      { snapshotNumber = 3
      , parties = healthyOnChainParties
      , contestationDeadline = posixFromUTCTime healthyContestationDeadline
      , contestationPeriod = healthyOnChainContestationPeriod
      , depositPeriod = DP.toChain dperiod
      , headId = toPlutusCurrencySymbol testPolicyId
      , contesters = []
      , version = toInteger healthyCloseSnapshotVersion
      , accumulatorCommitment =
          Accumulator.getAccumulatorCommitment
            (Accumulator.buildFromSnapshotUTxOs splitUTxOInHead (Just healthyDepositedUTxO) Nothing)
      , headAdaOverhead = 0
      }

healthyContestIncClosedHeadTxOut :: TxOut CtxUTxO
healthyContestIncClosedHeadTxOut =
  mkHeadOutput
    testNetworkId
    testPolicyId
    (verificationKeyToOnChainId <$> healthyParticipants)
    (mkTxOutDatumInline healthyContestIncClosedState)

healthyContestIncTx :: (Tx, UTxO)
healthyContestIncTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton healthyClosedHeadTxIn healthyContestIncClosedHeadTxOut
      <> registryUTxO scriptRegistry

  tx =
    contestTx
      scriptRegistry
      healthyContesterVerificationKey
      (mkHeadId testPolicyId)
      healthyContestationPeriod
      healthyCloseSnapshotVersion
      healthyContestIncSnapshot
      (healthyIncSignature healthyContestIncSnapshotNumber)
      (healthySlotNo, slotNoToUTCTime systemStart slotLength healthySlotNo)
      closedThreadOutput

  scriptRegistry = genScriptRegistry `generateWith` 42

  closedThreadOutput =
    ClosedThreadOutput
      { closedThreadUTxO = (healthyClosedHeadTxIn, healthyContestIncClosedHeadTxOut)
      , closedParties = healthyOnChainParties
      , closedContestationDeadline = posixFromUTCTime healthyContestationDeadline
      , closedContesters = []
      , closedHeadAdaOverhead = 0
      , closedDepositPeriod = DP.toChain dperiod
      }

healthyIncSignature :: SnapshotNumber -> MultiSignature (Snapshot Tx)
healthyIncSignature number =
  aggregate [sign sk healthyContestIncSnapshot{number} | sk <- healthySigningKeys]

data ContestIncMutation
  = ContestUnusedIncAlterRedeemerCommitHash
  | ContestUsedIncAlterAccumulatorCommitment
  | ContestUnusedIncAlterAccumulatorCommitment
  | ContestUsedIncMutateSnapshotVersion
  | ContestUnusedIncMutateSnapshotVersion
  deriving stock (Generic, Show, Enum, Bounded)

genContestIncMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestIncMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ContestUnusedIncAlterRedeemerCommitHash . ChangeHeadRedeemer <$> do
        mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
        pure $
          Head.Contest
            Head.ContestUnused
              { signature = toPlutusSignatures mutatedSignature
              , accumulatorHash = healthyContestIncAccumulatorHash
              }
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUsedIncAlterAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUnusedIncAlterAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUsedIncMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUnusedIncMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
