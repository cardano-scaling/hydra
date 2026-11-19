module Hydra.HeadLogic.Snapshot.Capacity (checkSnapshotCapacity, checkDecommitCapacity) where

import Hydra.HeadLogic.Limits (SnapshotLimits, checkOutputCount)
import Hydra.Ledger (ValidationError (ValidationError))
import Hydra.Prelude
import Hydra.Tx (
  IsTx,
  UTxOType,
  applyTxTo,
  combinedUTxO,
  outputsOfUTxO,
 )
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))

-- | Make sure the snapshot wouldn't exceed the maximum number of UTxOs.
checkSnapshotCapacity ::
  forall tx.
  IsTx tx =>
  SnapshotLimits ->
  UTxOType tx ->
  Maybe (UTxOType tx) ->
  Maybe (UTxOType tx) ->
  Either AccumulatorTooLarge ()
checkSnapshotCapacity limits active commit decommit = case (commit, decommit) of
  (Nothing, Nothing) -> check active
  _ -> check (combinedUTxO active Nothing decommit) *> check (combinedUTxO active commit Nothing)
 where
  check :: UTxOType tx -> Either AccumulatorTooLarge ()
  check utxo = checkOutputCount limits $ length (outputsOfUTxO utxo)

-- | Check that the decommit isn't too large.
checkDecommitCapacity ::
  IsTx tx =>
  SnapshotLimits ->
  UTxOType tx ->
  UTxOType tx ->
  tx ->
  Either ValidationError ()
checkDecommitCapacity limits validatedLocalAfter confirmed tx =
  first invalid $
    checkSnapshotCapacity limits validatedLocalAfter Nothing Nothing
      *> checkSnapshotCapacity limits (applyTxTo tx confirmed) Nothing Nothing
 where
  invalid AccumulatorTooLarge{utxoCount, maxAllowed} =
    ValidationError $
      "decommit exceeds the head's accumulator capacity: "
        <> show utxoCount
        <> " outputs (maximum "
        <> show maxAllowed
        <> ")"
