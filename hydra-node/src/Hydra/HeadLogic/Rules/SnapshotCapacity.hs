-- | Capacity of both fanout alternatives, without constructing commitments.
module Hydra.HeadLogic.Rules.SnapshotCapacity (checkSnapshotCapacity, checkDecommitCapacity) where

import Hydra.HeadLogic.Limits (SnapshotLimits, checkOutputCount)
import Hydra.Prelude

import Hydra.Ledger (ValidationError (ValidationError))
import Hydra.Tx (IsTx, UTxOType, applyTxTo, combinedUTxO, outputsOfUTxO)
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))

-- | Pending decommit outputs remain owed by the Head; pending commit outputs
-- enter the other fanout alternative. Count references after each union.
checkSnapshotCapacity :: forall tx. IsTx tx => SnapshotLimits -> UTxOType tx -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> Either AccumulatorTooLarge ()
checkSnapshotCapacity limits active commit decommit = case (commit, decommit) of
  (Nothing, Nothing) -> check active
  _ -> check (combinedUTxO active Nothing decommit) *> check (combinedUTxO active commit Nothing)
 where
  check :: UTxOType tx -> Either AccumulatorTooLarge ()
  check utxo = checkOutputCount limits $ length (outputsOfUTxO utxo)

-- | Check after ledger admission, before excluding decommit outputs. Pending
-- merges cannot supply confirmed space: a follower applies the decommit first.
checkDecommitCapacity :: IsTx tx => SnapshotLimits -> UTxOType tx -> UTxOType tx -> tx -> Either ValidationError ()
checkDecommitCapacity limits validatedLocalAfter confirmed tx =
  first invalid $
    checkSnapshotCapacity limits validatedLocalAfter Nothing Nothing
      *> checkSnapshotCapacity limits (applyTxTo tx confirmed) Nothing Nothing
 where
  invalid AccumulatorTooLarge{utxoCount, maxAllowed} =
    ValidationError $ "decommit exceeds the head's accumulator capacity: " <> show utxoCount <> " outputs (maximum " <> show maxAllowed <> ")"
