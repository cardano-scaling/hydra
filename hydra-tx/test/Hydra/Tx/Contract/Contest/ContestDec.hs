{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestDec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Data.Maybe (fromJust)

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)

import Hydra.Tx (Snapshot)
import Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyContestSnapshotNumber,
  healthySignature,
 )
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceOmegaUTxOHash,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, oneof, suchThat)
import Test.QuickCheck.Instances ()

data ContestDecMutation
  = ContestUsedDecAlterRedeemerDecommitHash
  | ContestUnusedDecAlterRedeemerDecommitHash
  | ContestUsedDecAlterDatumomegaUTxOHash
  | ContestUnusedDecAlterDatumomegaUTxOHash
  | ContestUsedDecMutateSnapshotVersion
  | ContestUnusedDecMutateSnapshotVersion
  deriving stock (Generic, Show, Enum, Bounded)

genContestDecMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestDecMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode FailedContestUsedDec) ContestUsedDecAlterRedeemerDecommitHash <$> do
        pure $
          ChangeHeadRedeemer $
            Head.Contest
              Head.ContestUsedDec
                { signature = toPlutusSignatures (healthySignature healthyContestSnapshotNumber)
                , alreadyDecommittedUTxOHash = mempty
                }
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ContestUnusedDecAlterRedeemerDecommitHash . ChangeHeadRedeemer <$> do
        mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
        pure $
          Head.Contest
            Head.ContestUnusedDec
              { signature = toPlutusSignatures mutatedSignature
              }
    , SomeMutation (pure $ toErrorCode FailedContestUsedDec) ContestUsedDecAlterRedeemerDecommitHash <$> do
        mutatedHash <- arbitrary `suchThat` (/= mempty)
        pure $
          ChangeHeadRedeemer $
            Head.Contest
              Head.ContestUsedDec
                { signature = toPlutusSignatures (healthySignature healthyContestSnapshotNumber)
                , alreadyDecommittedUTxOHash = mutatedHash
                }
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ContestUsedDecAlterDatumomegaUTxOHash . ChangeOutput 0 <$> do
        mutatedHash <- arbitrary `suchThat` (/= mempty)
        pure $ headTxOut & modifyInlineDatum (replaceOmegaUTxOHash mutatedHash)
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ContestUnusedDecAlterDatumomegaUTxOHash . ChangeOutput 0 <$> do
        mutatedHash <- arbitrary `suchThat` (/= mempty)
        pure $ headTxOut & modifyInlineDatum (replaceOmegaUTxOHash mutatedHash)
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUsedDecMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) ContestUnusedDecMutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
