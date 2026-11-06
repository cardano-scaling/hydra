{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Shared snapshot and deposit selection for leaders and followers.
module Hydra.HeadLogic.Rules.SnapshotRequest where

import Data.List (elemIndex, minimumBy)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.HeadLogic.Rules.SnapshotCapacity (checkSnapshotCapacity)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (finalizedCommit),
  OpenState (OpenState, coordinatedHeadState, headId),
 )
import Hydra.HeadLogic.State.Operations (
  absorbable,
  finalizedDepositTxId,
  maxTxsPerSnapshot,
  namesNoneOf,
  settledUTxO,
 )
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), PendingDeposits, depositsForHead)
import Hydra.Prelude
import Hydra.Tx (IsTx (..), TxIdType, UTxOType, applyTxTo, utxoFromTx, withoutUTxO)
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

-- | A selected deposit must still be registered and unexpired. Expiry retains
-- the entry for recovery, so the selected identifier alone is not enough.
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

-- | Acceptance against the optimistic local ledger is distinct from readiness
-- against the confirmed base. A carried decommit needs no new ledger application.
data DecommitStatus tx
  = NoDecommit
  | AwaitingInputs tx
  | ReadyToDecommit tx
  | CarryDecommit tx

decommitStatus :: IsTx tx => SnapshotVersion -> Snapshot tx -> Maybe tx -> DecommitStatus tx
decommitStatus headVersion snapshot = \case
  Nothing -> NoDecommit
  Just tx
    | headVersion == snapshot.version && snapshot.utxoToDecommit == Just (utxoFromTx tx) -> CarryDecommit tx
    | txInputsAvailable tx (settledUTxO headVersion snapshot) -> ReadyToDecommit tx
    | otherwise -> AwaitingInputs tx

pendingDecommit :: DecommitStatus tx -> Maybe tx
pendingDecommit = \case
  NoDecommit -> Nothing
  AwaitingInputs tx -> Just tx
  ReadyToDecommit tx -> Just tx
  CarryDecommit tx -> Just tx

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
-- A settling commit is carried while it can still land. Transactions colliding
-- with its outputs are held back. New decommits wait for their inputs to be
-- confirmed; followers still perform full ledger validation.
nextSnapshotRequest ::
  IsTx tx =>
  -- | The UTxO 'onOpenNetworkReqSn' applies the request's transactions to.
  UTxOType tx ->
  -- | Pending local transactions.
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  DecommitStatus tx ->
  -- | The commit the confirmed snapshot is settling, see 'settlingCommitAt'
  Maybe (SettlingCommit tx) ->
  SnapshotRequest tx
nextSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId decommit mSettlingCommit =
  fst (buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId decommit mSettlingCommit)

-- | 'nextSnapshotRequest', if worth sending right away: it carries a tx or a
-- new deposit, a newly eligible decommit, or it prunes a pending tx no node
-- accepts. A carried incremental action alone does not start another round.
chainedSnapshotRequest ::
  IsTx tx =>
  UTxOType tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  DecommitStatus tx ->
  Maybe (SettlingCommit tx) ->
  Maybe (SnapshotRequest tx)
chainedSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId decommit mSettlingCommit =
  request <$ guard worthChaining
 where
  (request, worthChaining) = buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId decommit mSettlingCommit

-- | The request and whether it is worth chaining, see 'chainedSnapshotRequest'.
buildSnapshotRequest ::
  forall tx.
  IsTx tx =>
  UTxOType tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  DecommitStatus tx ->
  Maybe (SettlingCommit tx) ->
  (SnapshotRequest tx, Bool)
buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId decommit mSettlingCommit =
  case mSettlingCommit of
    Just (ContinueCommit (depositTxId, Deposit{deposited})) ->
      let requestTxs = requestable (Just deposited) Nothing
       in (SnapshotRequest{requestTxs, requestDecommitTx = Nothing, requestDepositTxId = Just depositTxId}, hasPendingTransactions)
    Just DropCommit -> withoutDeposit
    Nothing ->
      case selectNextDeposit canAbsorb pendingDeposits currentDepositTxId (pendingDecommit decommit) of
        Just depositTxId -> (SnapshotRequest{requestTxs = requestable Nothing Nothing, requestDecommitTx = Nothing, requestDepositTxId = Just depositTxId}, True)
        Nothing -> withoutDeposit
 where
  withoutDeposit :: (SnapshotRequest tx, Bool)
  withoutDeposit =
    let (requestDecommitTx, newlyReady) = case decommit of
          ReadyToDecommit tx -> (Just tx, True)
          CarryDecommit tx -> (Just tx, False)
          _ -> (Nothing, False)
        requestTxs = requestable Nothing requestDecommitTx
     in (SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId = Nothing}, newlyReady || hasPendingTransactions)

  -- A new deposit must fit alongside the actual request. Defer it rather
  -- than blocking ordinary transactions; a settling commit is always carried.
  canAbsorb deposited =
    absorbable utxo candidates deposited
      && isRight (checkSnapshotCapacity requestedUTxO (Just deposited) Nothing)

  requestedUTxO = foldl' (flip applyTxTo) utxo (requestable Nothing Nothing)

  -- With pending transactions, the selected request either carries a prefix
  -- or prunes its first blocked transaction in that request's commit/decommit
  -- context. An incremental action alone does not justify another snapshot.
  hasPendingTransactions :: Bool
  hasPendingTransactions = not (null candidates)

  -- The pending txs up to the first one creating an output the ledger or the
  -- carried commit holds, or exceeding capacity. Check every prefix: a later
  -- merge might fall outside maxTxsPerSnapshot or arrive only after ReqSn.
  requestable :: Maybe (UTxOType tx) -> Maybe tx -> [tx]
  requestable mCommit mDecommit = go activeUTxO candidates
   where
    held = utxo <> fold mCommit
    mDecommitUTxO = utxoFromTx <$> mDecommit
    activeUTxO = case mDecommit of
      Just tx -> applyTxTo tx utxo `withoutUTxO` utxoFromTx tx
      Nothing -> utxo
    go _ [] = []
    go u (tx : rest)
      | namesNoneOf (utxoFromTx tx) held
      , let next = applyTxTo tx u
      , isRight (checkSnapshotCapacity next mCommit mDecommitUTxO) =
          tx : go next rest
      | otherwise = []

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
