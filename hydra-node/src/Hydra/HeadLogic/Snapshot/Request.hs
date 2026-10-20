{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Requesting the next snapshot as its leader.
--
-- Several handlers may propose a snapshot: a new transaction, a confirmed
-- snapshot, a recorded decommit, an activated deposit, and a settlement that
-- landed on chain. Each decides on its own whether to propose; what they send
-- when they do is built here, once.
module Hydra.HeadLogic.Snapshot.Request where

import Hydra.Prelude

import Data.List (elemIndex, minimumBy)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome,
  StateChanged (..),
  cause,
  newState,
  noop,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (..),
  SeenSnapshot (..),
  isCollectingAcks,
 )
import Hydra.Ledger (
  Ledger (..),
  ValidationError (..),
 )
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (
  Environment (..),
 )
import Hydra.Node.State (
  Deposit (..),
  DepositStatus (..),
  PendingDeposits,
 )
import Hydra.Tx (
  IsTx (..),
 )
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  getSnapshot,
 )

-- * The Coordinated Head protocol

-- | Maximum number of transaction ids per snapshot. This effectively limits our
-- "block size" and ensures it does not grow arbitrarily with the backlog of
-- pending transactions (localTxs). Only applied when requesting snapshots as
-- a leader; followers accept larger requests, so this can change without a
-- coordinated upgrade.
--
-- 4000 was chosen from backlog-drain sweeps at caps 1000 to 8000 (see
-- hydra-cluster/bench/BASELINES.md): per-round costs scale with the backlog,
-- so under sustained overload larger caps drain strictly faster (2.7x at
-- 4000 vs 1000 on a 24k-tx backlog) with lower confirmation latency and
-- lower peak memory, while the cap never binds under light load. Gains taper
-- beyond 4000 and worst-case round size keeps growing (ReqSn bytes, failed
-- round rework, follower reapply stall), so we stop at roughly 10x below
-- etcd's 1.5MiB request limit, which a too-large ReqSn would wedge on.
maxTxsPerSnapshot :: Int
maxTxsPerSnapshot = 4000

-- | If this node is the snapshot leader and there is anything to snapshot
-- (pending local transactions, a deposit to carry, or a decommit recorded
-- while the commit was in flight), request the next snapshot with the bumped
-- version after a commit or decommit finalises on-chain.
--
-- Guards:
--   * Only fires when 'newVersion' is ahead of the local 'version': this
--     avoids duplicate 'SnapshotRequestDecided' events when multiple parties
--     post the same on-chain tx and each posting produces a separate
--     finalisation observation, and it keeps a finalization re-landing after a
--     rollback (its 'newVersion' is at or behind the local version, which
--     never rolls back) from requesting a snapshot with a stale version.
--   * Skips when AckSns are already being collected ('SeenSnapshot'): the
--     node keeps collecting signatures. Once it confirms,
--     'maybeRequestNextSnapshot' requests the next snapshot with the bumped
--     version. Firing here uses stale 'localTxs' and can cause 'BadInputsUTxO'
--     on other parties. A party that receives the ReqSn after the bump can
--     sign one version behind when it preserves the confirmed action.
--   * Allows 'RequestedSnapshot': our request may still be in transit.
--     Requesting at the new version also covers a lost request; receivers
--     reject a second request for the same snapshot number as a duplicate.
--
-- The caller builds the request after removing the action that just settled.
-- A decommit recorded while a commit was in flight can now be proposed, even
-- without local transactions; a settled decommit must not be proposed again.
maybeRequestSnapshotAfterVersionBump ::
  IsTx tx =>
  HeadParameters ->
  Party ->
  SnapshotNumber ->
  -- | The request to make, if any, built for the new version. The caller
  -- excludes the incremental action that just settled.
  Maybe (SnapshotRequest tx) ->
  SnapshotVersion ->
  SnapshotVersion ->
  SeenSnapshot tx ->
  Outcome tx
maybeRequestSnapshotAfterVersionBump parameters party nextSn mRequest version newVersion seenSnapshot =
  case mRequest of
    Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}
      | isLeader parameters party nextSn && newVersion > version && not (isCollectingAcks seenSnapshot) ->
          newState SnapshotRequestDecided{snapshotNumber = nextSn}
            <> cause (NetworkEffect $ ReqSn newVersion nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)
    _ -> noop

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

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

-- | Prefer a deposit already tracked in 'currentDepositTxId' (if still pending).
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
settlingCommitAt Environment{depositPeriod} version chainTime pendingDeposits Snapshot{utxoToCommit, depositTxId, version = snapshotVersion}
  | version /= snapshotVersion || isNothing utxoToCommit = Nothing
  | otherwise = Just $ case (\claimed -> (claimed,) <$> Map.lookup claimed pendingDeposits) =<< depositTxId of
      Just claimed@(_, Deposit{deadline})
        | chainTime <= addUTCTime (toNominalDiffTime depositPeriod) deadline -> ContinueCommit claimed
      -- Also for snapshots without a 'depositTxId' (before 2.4.0).
      _ -> DropCommit

-- | The UTxO the next request is validated against: the snapshot in flight, if
-- any, else the confirmed one.
nextRequestUTxO :: IsTx tx => CoordinatedHeadState tx -> UTxOType tx
nextRequestUTxO CoordinatedHeadState{seenSnapshot, confirmedSnapshot, version} =
  case seenSnapshot of
    SeenSnapshot{snapshot} -> settledUTxO version snapshot
    _ -> settledUTxO version (getSnapshot confirmedSnapshot)

-- | Whether a request applying these txs to this UTxO can absorb the deposit:
-- it must name no output the UTxO holds or the txs create. Deposit datums are
-- not authenticated, so anyone can name existing outputs, and absorbing such a
-- deposit would count value the ledger does not have.
absorbable :: IsTx tx => UTxOType tx -> [tx] -> UTxOType tx -> Bool
absorbable utxo txs =
  let created = foldMap utxoFromTx txs
   in \deposited -> namesNoneOf deposited utxo && namesNoneOf deposited created

-- | Whether a deposit of the given outputs names none of the output references
-- the given UTxO holds.
namesNoneOf :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
namesNoneOf deposited utxo =
  -- NOTE: remove the UTxO from the deposit, not the deposit from the UTxO: the
  -- deposit is small, so this is cheaper.
  deposited `withoutUTxO` utxo == deposited

-- | Apply txs, refusing one that creates an output the UTxO already holds.
-- Impossible on L1, possible on L2: a deposit can name a tx's outputs before
-- that tx is submitted.
applyTransactionsWithoutCollision :: IsTx tx => Ledger tx -> ChainSlot -> UTxOType tx -> [tx] -> Either (tx, ValidationError) (UTxOType tx)
applyTransactionsWithoutCollision Ledger{applyTransactions = apply} slot =
  foldlM $ \utxo tx ->
    if namesNoneOf (utxoFromTx tx) utxo
      then apply slot utxo [tx]
      else Left (tx, ValidationError "transaction creates an output the head already holds")

-- | The 'SeenSnapshot' after a version bump. A snapshot already in flight is
-- kept so this node can collect the remaining signatures. Other states reset
-- to the confirmed snapshot number. A party that receives the 'ReqSn' after
-- the bump signs at the requested version if it preserves the settled action.
seenSnapshotAfterVersionBump :: IsTx tx => ConfirmedSnapshot tx -> SeenSnapshot tx -> SeenSnapshot tx
seenSnapshotAfterVersionBump confirmedSnapshot = \case
  seen@SeenSnapshot{} -> seen
  _ -> LastSeenSnapshot{lastSeen = (getSnapshot confirmedSnapshot).number}

-- | The spendable UTxO of a snapshot, given the head's current 'SnapshotVersion'.
-- A pending commit only becomes spendable once its increment has landed on
-- chain, which bumps the version past the snapshot's.
settledUTxO :: IsTx tx => SnapshotVersion -> Snapshot tx -> UTxOType tx
settledUTxO headVersion Snapshot{utxo, utxoToCommit, version}
  | headVersion > version = utxo <> fromMaybe mempty utxoToCommit
  | otherwise = utxo
