{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestDec where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude hiding (label)
import "hydra-test-utils" Test.Hydra.Prelude

import "base" Data.Maybe (fromJust)

import "hydra-plutus" Hydra.Contract.Error (toErrorCode)
import "hydra-plutus" Hydra.Contract.HeadError (HeadError (..))
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-tx" Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)

import "QuickCheck" Test.QuickCheck (arbitrarySizedNatural, oneof, suchThat)
import "hydra-tx" Hydra.Tx (Snapshot)
import "hydra-tx" Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyContestSnapshotNumber,
  healthySignature,
 )
import "hydra-tx" Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceOmegaUTxOHash,
  replaceSnapshotVersion,
 )
import "quickcheck-instances" Test.QuickCheck.Instances ()

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
