{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestUsedDec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Tx.Crypto (toPlutusSignatures)

import Hydra.Tx.Contract.Contest.Healthy (healthyContestSnapshotNumber, healthySignature)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceDeltaUTxOHash,
 )
import Test.QuickCheck (oneof, suchThat)
import Test.QuickCheck.Instances ()

data ContestUsedDecMutation
  = AlterRedeemerDecommitHash
  | AlterDatumDeltaUTxOHash
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
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
