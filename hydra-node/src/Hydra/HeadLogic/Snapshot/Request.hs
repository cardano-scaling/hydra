{-# LANGUAGE OverloadedRecordDot #-}

-- | Requesting the next snapshot as its leader.
--
-- Several handlers may propose a snapshot: a new transaction, a confirmed
-- snapshot, a recorded decommit, an activated deposit, and a settlement that
-- landed on chain. Each decides on its own whether to propose; what they send
-- when they do is built here, once.
module Hydra.HeadLogic.Snapshot.Request where

import Hydra.Prelude

import Data.List (elemIndex)
import Data.Sequence qualified as Seq
import Hydra.HeadLogic.Outcome (Effect (..), Outcome, StateChanged (..), cause, newState, noop)
import Hydra.HeadLogic.State (SeenSnapshot (..), isCollectingAcks)
import Hydra.Network.Message (Message (..))
import Hydra.Tx (IsTx (..), TxIdType, txId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

-- | Maximum number of transaction ids per snapshot. This effectively limits our
-- "block size" and ensures it does not grow arbitrarily with the backlog of
-- pending transactions (localTxs). Only applied when requesting snapshots as
-- a leader; followers accept larger requests, so this can change without a
-- coordinated upgrade.
--
-- 1000 was chosen from a sweep against 100/250 on sustained-load benchmarks
-- (see hydra-cluster/bench/BASELINES.md): per-round costs that scale with the
-- backlog dominate at small caps (4.6-5.7x lower throughput at 100 with a
-- deep backlog), while peak node memory was flat across the sweep.
maxTxsPerSnapshot :: Int
maxTxsPerSnapshot = 1000

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- | The 'ReqSn' this node multicasts as leader of the next snapshot: the
-- version to sign at, the snapshot number, the local transactions to carry,
-- capped at 'maxTxsPerSnapshot', and the incremental action to settle, a
-- decommit or a deposit, never both.
reqSnMessage ::
  IsTx tx =>
  SnapshotVersion ->
  SnapshotNumber ->
  -- | Local transactions, in application order.
  Seq tx ->
  -- | Decommit transaction to carry.
  Maybe tx ->
  -- | Deposit to carry.
  Maybe (TxIdType tx) ->
  Message tx
reqSnMessage version nextSn localTxs =
  ReqSn version nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs)

-- | Request the next snapshot: record that this node decided to, and multicast
-- the 'reqSnMessage'. The guard deciding whether to request stays with each
-- caller.
requestSnapshot ::
  IsTx tx =>
  SnapshotVersion ->
  SnapshotNumber ->
  -- | Local transactions, in application order.
  Seq tx ->
  -- | Decommit transaction to carry.
  Maybe tx ->
  -- | Deposit to carry.
  Maybe (TxIdType tx) ->
  Outcome tx
requestSnapshot version nextSn localTxs nextDecommitTx nextDeposit =
  -- XXX: This state update has no equivalence in the spec. Do we really need
  -- to store that we have requested a snapshot? If yes, should update spec.
  newState SnapshotRequestDecided{snapshotNumber = nextSn}
    <> cause (NetworkEffect $ reqSnMessage version nextSn localTxs nextDecommitTx nextDeposit)

-- | If this node is the snapshot leader and there is something to snapshot
-- (local transactions, a deposit to carry, or a decommit recorded while the
-- commit was in flight), request the next snapshot with the bumped version
-- after a commit or decommit lands on chain.
--
-- Guards:
--   * Only when 'newVersion' is ahead of our 'version'. Several parties post
--     the same transaction and each posting is observed, so this avoids one
--     'SnapshotRequestDecided' per observation. It also keeps a settlement
--     that landed again after a rollback (its 'newVersion' is at or behind our
--     version, which never goes down) from requesting a snapshot with a stale
--     version.
--   * Not while signatures are being collected ('SeenSnapshot'). That snapshot
--     will complete, because every party can sign it: those that saw the bump
--     before the ReqSn sign one version behind their own (see
--     'waitOnSnapshotVersion'), and 'maybeRequestNextSnapshot' then requests
--     the next one with the bumped version. Firing here would use stale
--     'localTxs' and cause 'BadInputsUTxO' on other parties.
--   * Allowed in 'RequestedSnapshot': our own request at the old version is
--     still on its way. Requesting again at the new version costs nothing,
--     since every party rejects a second request for the same number as a
--     duplicate, and it covers the case where the first one was lost.
--
-- The optional 'depositTxId' goes into the 'ReqSn': commit finalisation passes
-- 'Nothing', since the deposit is already included, and decommit finalisation
-- passes the next queued deposit if there is one. The optional decommit is one
-- recorded while the commit was in flight: every proposal carried the commit
-- first, and on a head with no local txs nothing else would propose it now
-- that the increment landed. Commit finalisation passes it; decommit
-- finalisation passes 'Nothing', since the decommit that settled is being
-- cleared and no other could be recorded meanwhile. A deposit wins over the
-- decommit, as in 'selectNextIncrementalAction'.
maybeRequestSnapshotAfterVersionBump ::
  IsTx tx =>
  HeadParameters ->
  Party ->
  SnapshotNumber ->
  Seq tx ->
  SnapshotVersion ->
  SnapshotVersion ->
  SeenSnapshot tx ->
  Maybe (TxIdType tx) ->
  Maybe tx ->
  Outcome tx
maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot depositTxId recordedDecommitTx =
  if isLeader parameters party nextSn && (not (null localTxs) || isJust nextDeposit || isJust nextDecommitTx) && newVersion > version && not (isCollectingAcks seenSnapshot)
    then requestSnapshot newVersion nextSn localTxs nextDecommitTx nextDeposit
    else noop
 where
  (nextDecommitTx, nextDeposit) = case depositTxId of
    Just _ -> (Nothing, depositTxId)
    Nothing -> (recordedDecommitTx, Nothing)

-- | The 'SeenSnapshot' after a version bump. A snapshot already in flight is
-- kept: every party can still sign it, those that saw the bump before the
-- 'ReqSn' one version behind their own (see 'waitOnSnapshotVersion'), so it
-- completes and 'maybeRequestNextSnapshot' requests the next one with the
-- bumped version. Only when nothing is in flight is it reset. (A local
-- 'SeenSnapshot' only proves this party echoed the 'ReqSn', not that every
-- party did.)
seenSnapshotAfterVersionBump :: IsTx tx => ConfirmedSnapshot tx -> SeenSnapshot tx -> SeenSnapshot tx
seenSnapshotAfterVersionBump confirmedSnapshot = \case
  seen@SeenSnapshot{} -> seen
  _ -> LastSeenSnapshot{lastSeen = (getSnapshot confirmedSnapshot).number}
