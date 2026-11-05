-- | Capacity of both fanout alternatives, without constructing commitments.
module Hydra.HeadLogic.Rules.SnapshotCapacity (checkSnapshotCapacity, checkDecommitCapacity) where

import Hydra.Prelude

import Hydra.Ledger (ValidationError (ValidationError))
import Hydra.Tx (IsTx, UTxOType, applyTxTo, combinedUTxO, outputsOfUTxO)
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..), maxAccumulatorSize)

-- | Pending decommit outputs remain owed by the Head; pending commit outputs
-- enter the other fanout alternative. Count references after each union.
checkSnapshotCapacity :: forall tx. IsTx tx => UTxOType tx -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> Either AccumulatorTooLarge ()
checkSnapshotCapacity active commit decommit = case (commit, decommit) of
  (Nothing, Nothing) -> check active
  _ -> check (combinedUTxO active Nothing decommit) *> check (combinedUTxO active commit Nothing)
 where
  check :: UTxOType tx -> Either AccumulatorTooLarge ()
  check utxo
    | count > maxAccumulatorSize = Left AccumulatorTooLarge{utxoCount = count, maxAllowed = maxAccumulatorSize}
    | otherwise = Right ()
   where
    count = length (outputsOfUTxO utxo)

-- | Check after ledger admission, before excluding decommit outputs. Pending
-- merges cannot supply confirmed space: a follower applies the decommit first.
checkDecommitCapacity :: IsTx tx => UTxOType tx -> UTxOType tx -> tx -> Either ValidationError ()
checkDecommitCapacity validatedLocalAfter confirmed tx =
  first invalid $
    checkSnapshotCapacity validatedLocalAfter Nothing Nothing
      *> checkSnapshotCapacity (applyTxTo tx confirmed) Nothing Nothing
 where
  invalid AccumulatorTooLarge{utxoCount, maxAllowed} =
    ValidationError $ "decommit exceeds the head's accumulator capacity: " <> show utxoCount <> " outputs (maximum " <> show maxAllowed <> ")"
