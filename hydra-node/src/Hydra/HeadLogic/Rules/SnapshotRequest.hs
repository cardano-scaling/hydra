{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Shared snapshot and deposit selection for leaders and followers.
module Hydra.HeadLogic.Rules.SnapshotRequest (
  SnapshotRequest,
  requestNumber,
  requestTxs,
  requestDecommitTx,
  requestDepositTxId,
  requestMessage,
  requestMatches,
  DecommitStatus (..),
  SettlingCommit (..),
  isLeader,
  existingDeposit,
  nextActiveDepositId,
  selectNextDeposit,
  decommitStatus,
  nextSnapshotRequest,
  chainedSnapshotRequest,
  buildSnapshotRequest,
  scopedDeposits,
  settlingCommitAt,
  settlingCommitAtPeriod,
) where

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
import Hydra.Network.Message (Message (ReqSn))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), PendingDeposits, depositsForHead)
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx (..), TxIdType, UTxOType, utxoFromTx, withoutUTxO)
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

-- | A capacity-checked proposal for a particular snapshot version and number.
-- The selector is the only constructor; callers cannot change its contents.
-- Capacity does not establish ledger validity, which followers check separately.
data SnapshotRequest tx = SnapshotRequest
  { selectedHeadId :: HeadId
  , selectedVersion :: SnapshotVersion
  , selectedNumber :: SnapshotNumber
  , selectedTransactions :: [tx]
  , selectedDecommit :: Maybe tx
  , selectedDeposit :: Maybe (TxIdType tx)
  }

-- Ordinary accessors keep record updates from bypassing the selector.
requestNumber :: SnapshotRequest tx -> SnapshotNumber
requestNumber = selectedNumber
requestTxs :: SnapshotRequest tx -> [tx]
requestTxs = selectedTransactions
requestDecommitTx :: SnapshotRequest tx -> Maybe tx
requestDecommitTx = selectedDecommit
requestDepositTxId :: SnapshotRequest tx -> Maybe (TxIdType tx)
requestDepositTxId = selectedDeposit

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

-- | Construct a proposal, including an empty round when requested by the caller
-- (for example, to drop an expired settling commit). Both fanout alternatives
-- must fit before a request can leave the selector.
nextSnapshotRequest ::
  IsTx tx =>
  SnapshotVersion ->
  Snapshot tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  DecommitStatus tx ->
  Maybe (SettlingCommit tx) ->
  Maybe (SnapshotRequest tx)
nextSnapshotRequest version previous txs deposits selected decommit settling =
  fst <$> buildSnapshotRequest version previous txs deposits selected decommit settling

-- | Start another round for transactions, a new deposit, a newly ready decommit,
-- or pruning blocked work. Carrying an incremental action alone is insufficient.
chainedSnapshotRequest ::
  IsTx tx =>
  SnapshotVersion ->
  Snapshot tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  DecommitStatus tx ->
  Maybe (SettlingCommit tx) ->
  Maybe (SnapshotRequest tx)
chainedSnapshotRequest version previous txs deposits selected decommit settling = do
  (request, useful) <- buildSnapshotRequest version previous txs deposits selected decommit settling
  request <$ guard useful

-- | Select the longest admissible prefix within the transaction cap. A later
-- merge cannot make an overflowing prefix requestable. An empty request prunes
-- the blocked transaction, allowing independent work behind it to proceed.
buildSnapshotRequest ::
  forall tx.
  IsTx tx =>
  SnapshotVersion ->
  Snapshot tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  DecommitStatus tx ->
  Maybe (SettlingCommit tx) ->
  Maybe (SnapshotRequest tx, Bool)
buildSnapshotRequest version previous localTxs pendingDeposits currentDepositTxId decommit settling =
  case settling of
    Just (ContinueCommit (depositId, Deposit{deposited})) ->
      build (Just (depositId, deposited)) Nothing hasPending
    Just DropCommit -> withoutDeposit
    Nothing -> case selectNextDeposit canAbsorb pendingDeposits currentDepositTxId (pendingDecommit decommit) of
      Just depositId
        | Just Deposit{deposited} <- Map.lookup depositId pendingDeposits ->
            -- A new deposit can use space freed by this request's transactions.
            -- It is not part of the active ledger on which those transactions run.
            let (txs, after) = ordinaryRequest
             in (makeRequest txs Nothing (Just depositId), True)
                  <$ guard (isRight $ checkSnapshotCapacity after (Just deposited) Nothing)
      _ -> withoutDeposit
 where
  utxo = settledUTxO version previous
  candidates = toList (Seq.take maxTxsPerSnapshot localTxs)
  hasPending = not (null candidates)
  withoutDeposit = case decommit of
    ReadyToDecommit tx -> build Nothing (Just tx) True
    CarryDecommit tx -> build Nothing (Just tx) hasPending
    _ -> build Nothing Nothing hasPending

  -- A new deposit must fit beside the actual request. Deposits that do not fit
  -- wait; they do not prevent ordinary transactions from being requested.
  canAbsorb deposited =
    absorbable utxo candidates deposited
      && isRight (checkSnapshotCapacity requestedUTxO (Just deposited) Nothing)
  ordinaryRequest = requestable utxo Nothing Nothing
  requestedUTxO = snd ordinaryRequest

  build commit decommitTx useful = do
    let commitUTxO = snd <$> commit
        decommitUTxO = utxoFromTx <$> decommitTx
        active = case decommitTx of
          Just tx -> applyTxTo tx utxo `withoutUTxO` utxoFromTx tx
          Nothing -> utxo
    guard $ isRight (checkSnapshotCapacity active commitUTxO decommitUTxO)
    let (txs, _) = requestable active commitUTxO decommitUTxO
    pure (makeRequest txs decommitTx (fst <$> commit), useful)

  makeRequest = SnapshotRequest previous.headId version (previous.number + 1)

  requestable active commit decommitUTxO = go active candidates
   where
    held = utxo <> fold commit
    go u [] = ([], u)
    go u (tx : rest)
      | namesNoneOf (utxoFromTx tx) held
      , let next = applyTxTo tx u
      , isRight (checkSnapshotCapacity next commit decommitUTxO) =
          let (txs, after) = go next rest in (tx : txs, after)
      | otherwise = ([], u)

-- | The message and retained request marker derive from the same proposal.
requestMessage :: IsTx tx => SnapshotRequest tx -> Message tx
requestMessage request =
  ReqSn
    request.selectedVersion
    request.selectedNumber
    (txId <$> request.selectedTransactions)
    request.selectedDecommit
    request.selectedDeposit

-- | A proposal is consumed in the invocation that selects it. These coordinates
-- also prevent retaining it across a confirmation, version change or new Head.
requestMatches :: HeadId -> SnapshotVersion -> SnapshotNumber -> SnapshotRequest tx -> Bool
requestMatches headId version confirmedNumber request =
  request.selectedHeadId == headId && request.selectedVersion == version && request.selectedNumber == confirmedNumber + 1

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
