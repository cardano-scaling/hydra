{-# LANGUAGE OverloadedRecordDot #-}

-- | Frozen pre-program ReqTx handler for migration comparisons and benchmarks.
-- Shared ledger and snapshot helpers remain production definitions; this oracle
-- checks the handler's decisions and ordering, not those helpers independently.
module Test.Hydra.HeadLogic.LegacyReqTx (legacyOnOpenNetworkReqTx) where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  OpenState (..),
  Outcome,
  SettlingCommit (..),
  SnapshotRequest (..),
  StateChanged (..),
  WaitReason (..),
  applyTransactionsWithoutCollision,
  chainedSnapshotRequest,
  isLeader,
  namesNoneOf,
  settledUTxO,
  settlingCommitAt,
  snapshotInFlight,
 )
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (cause, newState, wait)
import Hydra.Ledger (Ledger, ValidationError (..))
import Hydra.Network.Message (Message (ReqSn))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), PendingDeposits)
import Hydra.Tx (IsTx (..), UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, getSnapshot)

legacyOnOpenNetworkReqTx ::
  forall tx.
  IsTx tx =>
  Environment ->
  Ledger tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  tx ->
  Outcome tx
legacyOnOpenNetworkReqTx env ledger ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx =
  (newState TransactionReceived{tx} <>) $
    waitApplyTx $
      newState TransactionAppliedToLocalUTxO{headId, tx}
        & maybeRequestSnapshot (confirmedSn + 1)
 where
  waitApplyTx :: Outcome tx -> Outcome tx
  waitApplyTx cont =
    case applyTransactionsWithoutCollision ledger currentSlot localUTxO [tx] of
      Right _
        | namesNoneOf (utxoFromTx tx) namedByDepositsInPlay -> cont
        | otherwise -> notApplicable (ValidationError "transaction creates an output a pending deposit names")
      Left (_, err) -> notApplicable err
   where
    namedByDepositsInPlay :: UTxOType tx
    namedByDepositsInPlay =
      foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
        <> case settlingCommit of
          Just (ContinueCommit (_, Deposit{deposited})) -> deposited
          _ -> mempty

    notApplicable :: ValidationError -> Outcome tx
    notApplicable err
      | ttl > 0 = wait (WaitOnNotApplicableTx err)
      | otherwise = newState TxInvalid{headId, utxo = localUTxO, transaction = tx, validationError = err}

  maybeRequestSnapshot :: SnapshotNumber -> Outcome tx -> Outcome tx
  maybeRequestSnapshot nextSn outcome =
    case chainedSnapshotRequest confirmedUTxO localTxs' pendingDeposits currentDepositTxId decommitTx settlingCommit of
      Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}
        | not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn ->
            outcome
              <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
              <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)
      _ -> outcome

  Environment{party} = env

  CoordinatedHeadState
    { localTxs
    , localUTxO
    , confirmedSnapshot
    , seenSnapshot
    , decommitTx
    , version
    , currentDepositTxId
    } = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)

  settlingCommit = settlingCommitAt env version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot)

  OpenState{coordinatedHeadState, headId, parameters} = st

  localTxs' = localTxs Seq.|> tx
