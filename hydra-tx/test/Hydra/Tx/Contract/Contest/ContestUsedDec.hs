{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestUsedDec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Tx.Crypto (toPlutusSignatures)

import Hydra.Tx.Contract.Contest.Healthy (
  healthyCloseSnapshotVersion,
  healthyContestSnapshotNumber,
  healthySignature,
 )
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceDeltaUTxOHash,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, oneof, suchThat)
import Test.QuickCheck.Instances ()

data ContestUsedDecMutation
  = AlterRedeemerDecommitHash
  | AlterDatumDeltaUTxOHash
  | MutateSnapshotVersion
  deriving stock (Generic, Show, Enum, Bounded)

genContestUsedDecMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestUsedDecMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode FailedContestUsedDec) AlterRedeemerDecommitHash <$> do
        pure $
          ChangeHeadRedeemer $
            Head.Contest
              Head.ContestUsedDec
                { signature = toPlutusSignatures (healthySignature healthyContestSnapshotNumber)
                , alreadyDecommittedUTxOHash = mempty
                }
    , SomeMutation (pure $ toErrorCode FailedContestUsedDec) AlterRedeemerDecommitHash <$> do
        mutatedHash <- arbitrary `suchThat` (/= mempty)
        pure $
          ChangeHeadRedeemer $
            Head.Contest
              Head.ContestUsedDec
                { signature = toPlutusSignatures (healthySignature healthyContestSnapshotNumber)
                , alreadyDecommittedUTxOHash = mutatedHash
                }
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) AlterDatumDeltaUTxOHash . ChangeOutput 0 <$> do
        mutatedHash <- arbitrary `suchThat` (/= mempty)
        pure $ headTxOut & modifyInlineDatum (replaceDeltaUTxOHash mutatedHash)
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) MutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseSnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
