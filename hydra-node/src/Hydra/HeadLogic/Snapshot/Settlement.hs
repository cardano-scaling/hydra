{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Snapshot.Settlement where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.State (FinalizedSnapshot (..))
import Hydra.Prelude
import Hydra.Tx (IsTx (TxIdType, UTxOType, utxoFromTx))
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotVersion,
  getSnapshot,
 )

-- | This version still authorises a decrement, even without a local ReqDec.
settlingDecommit :: IsTx tx => SnapshotVersion -> ConfirmedSnapshot tx -> Bool
settlingDecommit version confirmed =
  let snapshot = getSnapshot confirmed
   in snapshot.version == version && isJust snapshot.utxoToDecommit

-- | ReqSn identifies a carried decommit by its non-empty output UTxO. On
-- Cardano these output references include the transaction's body hash.
matchesSnapshotDecommit :: IsTx tx => Snapshot tx -> tx -> Bool
matchesSnapshotDecommit snapshot tx =
  utxoFromTx tx /= mempty && snapshot.utxoToDecommit == Just (utxoFromTx tx)

-- | A confirmation can arrive after its increment/decrement observation. Keep
-- it for reposting if that observation rolls back. Shared with side-loading.
retainConfirmedSettlement :: IsTx tx => ChainSlot -> SnapshotVersion -> ConfirmedSnapshot tx -> Bool -> Maybe (FinalizedSnapshot tx) -> Maybe (FinalizedSnapshot tx)
retainConfirmedSettlement slot version confirmed settles previous
  | version == (getSnapshot confirmed).version + 1 && settles = Just $ FinalizedSnapshot confirmed slot
  | otherwise = previous

-- | The deposit claimed by a retained finalized increment ('finalizedCommit'),
-- if any. Such a deposit only resurfaces in 'pendingDeposits' when a rollback
-- erased its increment, and it is settled solely by re-posting that increment
-- ('maybeRepostIncrementTx'): it must never be proposed for a snapshot again
-- (the deposited funds are already counted in the head) nor recovered (that
-- would corrupt the L2 ledger). See #2741.
finalizedDepositTxId :: IsTx tx => Maybe (FinalizedSnapshot tx) -> Maybe (TxIdType tx)
finalizedDepositTxId finalizedCommit = do
  FinalizedSnapshot{snapshot} <- finalizedCommit
  (getSnapshot snapshot).depositTxId

-- | Update the retained 'finalizedCommit' on (re-)observation of an increment
-- claiming @depositTxId@:
--
--   * the locally confirmed snapshot claims it: retain that snapshot — only it
--     can re-claim the deposit if a rollback erases the increment, and
--     'confirmedSnapshot' may advance past it;
--   * the already retained snapshot claims it: the increment re-landed (e.g.
--     re-posted after a rollback) — re-stamp the observation slot, so a
--     rollback of the re-landed increment still triggers a re-post;
--   * neither: the observation raced local snapshot confirmation (another
--     party collected the last AckSn and posted first) — retention happens
--     once the snapshot confirms, see the 'SnapshotConfirmed' branch of
--     the snapshot coordination machine.
--
-- See #2741.
retainFinalizedCommit ::
  IsTx tx =>
  ChainSlot ->
  TxIdType tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  Maybe (FinalizedSnapshot tx)
retainFinalizedCommit slot depositTxId confirmedSnapshot retained
  | claims confirmedSnapshot = Just FinalizedSnapshot{snapshot = confirmedSnapshot, observedAtSlot = slot}
  | Just f@FinalizedSnapshot{snapshot} <- retained, claims snapshot = Just f{observedAtSlot = slot}
  | otherwise = retained
 where
  claims s = (getSnapshot s).depositTxId == Just depositTxId

-- | Update the retained 'finalizedDecommit' on (re-)observation of a decrement
-- bumping to @newVersion@; the counterpart of 'retainFinalizedCommit'. A
-- snapshot is tied to the decrement by identity, not by merely carrying a
-- decommit: only the decrement of that very snapshot bumps the version to
-- exactly one past the snapshot's (with a second decommit racing, the locally
-- confirmed snapshot may still be the previous, already settled one).
retainFinalizedDecommit ::
  IsTx tx =>
  ChainSlot ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  Maybe (FinalizedSnapshot tx)
retainFinalizedDecommit slot newVersion confirmedSnapshot retained
  | settles confirmedSnapshot = Just FinalizedSnapshot{snapshot = confirmedSnapshot, observedAtSlot = slot}
  | Just f@FinalizedSnapshot{snapshot} <- retained, settles snapshot = Just f{observedAtSlot = slot}
  | otherwise = retained
 where
  settles s = isJust (getSnapshot s).utxoToDecommit && (getSnapshot s).version + 1 == newVersion

-- | The spendable UTxO of a snapshot, given the head's current 'SnapshotVersion'.
-- A pending commit only becomes spendable once its increment has landed on
-- chain, which bumps the version past the snapshot's.
settledUTxO :: IsTx tx => SnapshotVersion -> Snapshot tx -> UTxOType tx
settledUTxO headVersion Snapshot{utxo, utxoToCommit, version}
  | headVersion > version = utxo <> fromMaybe mempty utxoToCommit
  | otherwise = utxo
