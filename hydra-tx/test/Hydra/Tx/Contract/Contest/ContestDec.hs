{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestDec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Maybe (fromJust)

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyContestSnapshot,
 )
import Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Tx.Snapshot (Snapshot (..))
import PlutusLedgerApi.V3 (toBuiltin)
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

data ContestDecMutation
  = ContestUnusedDecAlterRedeemerDecommitHash
  | ContestUsedDecAlterAccumulatorCommitment
  | ContestUnusedDecAlterAccumulatorCommitment
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
            Head.ContestUnusedDec
              { signature = toPlutusSignatures mutatedSignature
              , accumulatorHash = healthyContestAccumulatorHash
              }
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUsedDecAlterAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) ContestUnusedDecAlterAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUsedDecMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUnusedDecMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
