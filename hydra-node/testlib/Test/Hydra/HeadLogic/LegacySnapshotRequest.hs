{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen snapshot selection before decommit eligibility and wake-up fixes.
module Test.Hydra.HeadLogic.LegacySnapshotRequest where

import Data.List (elemIndex, minimumBy)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.HeadLogic.State (
  CoordinatedHeadState (finalizedCommit),
  OpenState (OpenState, coordinatedHeadState, headId),
 )
import Hydra.HeadLogic.State.Operations (
  absorbable,
  finalizedDepositTxId,
  maxTxsPerSnapshot,
  namesNoneOf,
 )
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), PendingDeposits, depositsForHead)
import Hydra.Prelude
import Hydra.Tx (IsTx (..), TxIdType, UTxOType, utxoFromTx)
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- ** Closing the Head

-- | The pending deposit that a local 'currentDepositTxId' still refers to, if any: the deposit must
--   be registered in 'pendingDeposits' and not 'Expired'.
--
--   Being registered and unexpired is what makes a recorded deposit id something the head may still
--   act on, and nothing else should be treated as a commit in flight. Neither of the two ways a
--   deposit stops being pending clears 'currentDepositTxId': 'DepositExpired' deliberately keeps the
--   deposit in the map so it can still be recovered, and 'DepositRecovered' only deletes the map
--   entry. So a caller that reads 'currentDepositTxId' on its own can end up waiting on a deposit
--   that is unclaimable, or already gone, and that wait never resolves.
existingDeposit :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx) -> Maybe (TxIdType tx, Deposit tx)
existingDeposit pendingDeposits currentDeposit =
  case currentDeposit of
    Nothing -> Nothing
    Just depositTxId ->
      case Map.lookup depositTxId pendingDeposits of
        Nothing -> Nothing
        Just deposit
          | deposit.status == Expired -> Nothing
          | otherwise -> Just (depositTxId, deposit)

-- | Find the oldest non-empty active deposit the request being built can
-- absorb ('absorbable'), if any. Deposits are selected in FIFO order by their
-- 'created' timestamp.
nextActiveDepositId :: IsTx tx => (UTxOType tx -> Bool) -> PendingDeposits tx -> Maybe (TxIdType tx)
nextActiveDepositId absorbableHere deposits =
  case filter (\(_, Deposit{deposited, status}) -> deposited /= mempty && status == Active && absorbableHere deposited) (Map.toList deposits) of
    [] -> Nothing
    xs -> Just (fst (minimumBy (comparing ((.created) . snd)) xs))

-- | Select the deposit to include in the next snapshot.
--
-- Prefers a deposit already tracked in 'currentDepositTxId' (if still pending).
-- Falls back to the oldest active deposit from 'pendingDeposits' the request
-- can absorb ('absorbable'), but only when neither a decommit is pending nor
-- the last confirmed snapshot's commit is still settling (to avoid
-- double-posting 'IncrementTx' before 'CommitFinalized' removes the deposit).
selectNextDeposit ::
  IsTx tx =>
  -- | Whether the request being built can absorb a deposit of these outputs,
  -- see 'absorbable'.
  (UTxOType tx -> Bool) ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  Maybe (TxIdType tx)
selectNextDeposit absorbableHere pendingDeposits currentDepositTxId mDecommitTx =
  (existingDeposit pendingDeposits currentDepositTxId >>= tracked)
    <|> (guard (isNothing mDecommitTx) *> nextActiveDepositId absorbableHere pendingDeposits)
 where
  tracked (depositTxId, Deposit{deposited}) = depositTxId <$ guard (deposited /= mempty && absorbableHere deposited)

-- | What a leader puts in its next 'ReqSn', see 'nextSnapshotRequest'.
data SnapshotRequest tx = SnapshotRequest
  { requestTxs :: [tx]
  , requestDecommitTx :: Maybe tx
  , requestDepositTxId :: Maybe (TxIdType tx)
  }

-- | The content of a leader's next 'ReqSn': the transactions to carry and the
-- incremental action, a commit or a decommit, never both. A snapshot carrying
-- both cannot be closed, since close and fanout express a single incremental
-- action ('setIncrementalActionMaybe').
--
-- A commit wins: its deposit expires on-chain, while a decommit only waits. This
-- cannot starve the decommit, because 'selectNextDeposit' refuses to start a
-- *new* commit while a decommit is pending — only one already in flight can win,
-- and that one stops being selected once it settles and leaves 'pendingDeposits'.
--
-- Leaders never build a request followers reject: a leader's own rejection
-- would stall the head. A settling commit is always carried; txs colliding
-- with it are held back.
nextSnapshotRequest ::
  IsTx tx =>
  -- | The UTxO 'onOpenNetworkReqSn' applies the request's transactions to.
  UTxOType tx ->
  -- | Pending local transactions.
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  -- | The commit the confirmed snapshot is settling, see 'settlingCommitAt'
  Maybe (SettlingCommit tx) ->
  SnapshotRequest tx
nextSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit =
  fst (buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit)

-- | 'nextSnapshotRequest', if worth sending right away: it carries a tx or a
-- new deposit, or it prunes a pending tx no node accepts. A request carrying
-- only a settling commit or a decommit is not: it would be re-requested
-- forever at the same version.
chainedSnapshotRequest ::
  IsTx tx =>
  UTxOType tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  Maybe tx ->
  Maybe (SettlingCommit tx) ->
  Maybe (SnapshotRequest tx)
chainedSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit =
  request <$ guard worthChaining
 where
  (request, worthChaining) = buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit

-- | The request and whether it is worth chaining, see 'chainedSnapshotRequest'.
buildSnapshotRequest ::
  forall tx.
  IsTx tx =>
  UTxOType tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  Maybe tx ->
  Maybe (SettlingCommit tx) ->
  (SnapshotRequest tx, Bool)
buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit =
  case mSettlingCommit of
    Just (ContinueCommit (depositTxId, Deposit{deposited})) ->
      let requestTxs = requestable (utxo <> deposited)
       in (SnapshotRequest{requestTxs, requestDecommitTx = Nothing, requestDepositTxId = Just depositTxId}, not (null requestTxs) || prunesPending)
    Just DropCommit -> withoutDeposit
    Nothing ->
      case selectNextDeposit (absorbable utxo candidates) pendingDeposits currentDepositTxId mDecommitTx of
        Just depositTxId -> (SnapshotRequest{requestTxs = requestable utxo, requestDecommitTx = Nothing, requestDepositTxId = Just depositTxId}, True)
        Nothing -> withoutDeposit
 where
  withoutDeposit :: (SnapshotRequest tx, Bool)
  withoutDeposit =
    let requestTxs = requestable utxo
     in (SnapshotRequest{requestTxs, requestDecommitTx = mDecommitTx, requestDepositTxId = Nothing}, not (null requestTxs) || prunesPending)

  -- The first pending tx collides with the ledger; only a snapshot prunes it.
  prunesPending :: Bool
  prunesPending = case candidates of
    tx : _ -> not (namesNoneOf (utxoFromTx tx) utxo)
    [] -> False

  -- The pending txs up to the first one creating an output the ledger or the
  -- carried commit holds: no node accepts that one.
  requestable :: UTxOType tx -> [tx]
  requestable held = takeWhile (\tx -> namesNoneOf (utxoFromTx tx) held) candidates

  -- The first 'maxTxsPerSnapshot' pending txs: a snapshot request lists at
  -- most that many.
  candidates :: [tx]
  candidates = toList (Seq.take maxTxsPerSnapshot localTxs)

-- | The deposits the handlers of an open head may act on: scoped to the head,
-- excluding the deposit claimed by the retained finalized increment (see
-- 'finalizedDepositTxId').
scopedDeposits :: IsTx tx => OpenState tx -> PendingDeposits tx -> PendingDeposits tx
scopedDeposits OpenState{headId, coordinatedHeadState} =
  maybe id Map.delete (finalizedDepositTxId coordinatedHeadState.finalizedCommit) . depositsForHead headId

-- | What the next request does with a commit the confirmed snapshot claims but
-- whose increment has not landed. Leaders and followers decide it the same way.
data SettlingCommit tx
  = -- | The increment may still land: every request must carry the commit.
    ContinueCommit (TxIdType tx, Deposit tx)
  | -- | The increment can no longer land: the deposit was recovered, or its
    -- deadline passed by more than 'depositPeriod' (a margin for rollbacks).
    -- 'Expired' is not enough: it is set before the deadline.
    DropCommit

settlingCommitAt :: IsTx tx => Environment -> SnapshotVersion -> UTCTime -> PendingDeposits tx -> Snapshot tx -> Maybe (SettlingCommit tx)
settlingCommitAt Environment{depositPeriod} = settlingCommitAtPeriod depositPeriod

settlingCommitAtPeriod :: IsTx tx => DepositPeriod -> SnapshotVersion -> UTCTime -> PendingDeposits tx -> Snapshot tx -> Maybe (SettlingCommit tx)
settlingCommitAtPeriod depositPeriod version chainTime pendingDeposits Snapshot{utxoToCommit, depositTxId, version = snapshotVersion}
  | version /= snapshotVersion || isNothing utxoToCommit = Nothing
  | otherwise = Just $ case (\claimed -> (claimed,) <$> Map.lookup claimed pendingDeposits) =<< depositTxId of
      Just claimed@(_, Deposit{deadline})
        | chainTime <= addUTCTime (toNominalDiffTime depositPeriod) deadline -> ContinueCommit claimed
      -- Also for snapshots without a 'depositTxId' (before 2.4.0).
      _ -> DropCommit
