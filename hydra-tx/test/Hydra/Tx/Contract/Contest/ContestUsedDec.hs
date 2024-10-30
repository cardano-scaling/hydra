{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.ContestUsedDec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Tx.Crypto (toPlutusSignatures)

import Hydra.Tx.Contract.Contest.Healthy (healthyContestSnapshotNumber, healthySignature)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
 )
import Test.QuickCheck.Instances ()

data ContestUsedDecMutation
  = AlterRedeemerDecommitHash
  deriving stock (Generic, Show, Enum, Bounded)

genContestUsedDecMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestUsedDecMutation (_tx, _utxo) =
  SomeMutation (pure $ toErrorCode FailedContestUsedDec) AlterRedeemerDecommitHash <$> do
    pure $
      ChangeHeadRedeemer $
        Head.Contest
          Head.ContestUsedDec
            { signature = toPlutusSignatures (healthySignature healthyContestSnapshotNumber)
            , alreadyDecommittedUTxOHash = mempty
            }
