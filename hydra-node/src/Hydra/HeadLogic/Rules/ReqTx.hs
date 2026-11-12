{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Receive and apply a transaction. Programs inside a transition keep related
-- guards and effects together; crem constrains transitions between phases.
module Hydra.HeadLogic.Rules.ReqTx (Phase (..), ReqTxTopology, machine) where

import Hydra.Prelude hiding (State, state)

import Crem.Topology (
  Topology (Topology),
 )
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (WaitReason (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.SnapshotRequest (SettlingCommit (..), chainedSnapshotRequest, decommitStatus, isLeader, settlingCommitAtPeriod)
import Hydra.HeadLogic.Rules.Transactions (checkTransactionOutputs)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, parameters),
  SeenSnapshot (SeenSnapshot),
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Operations (
  matchesSnapshotDecommit,
  namesNoneOf,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
  workflow,
 )
import Hydra.Ledger (ValidationError (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), PendingDeposits)
import Hydra.Tx (UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data Phase = CheckDuplicate | CheckOutputs | CheckDeposits | SelectSnapshot | RetryOrInvalidate | Finished

type ReqTxTopology =
  'Topology
    '[ '( 'CheckDuplicate, '[ 'CheckOutputs, 'Finished])
     , '( 'CheckOutputs, '[ 'CheckDeposits, 'RetryOrInvalidate])
     , '( 'CheckDeposits, '[ 'SelectSnapshot, 'RetryOrInvalidate])
     , '( 'SelectSnapshot, '[ 'Finished])
     , '( 'RetryOrInvalidate, '[ 'Finished])
     ]

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase

data State tx phase where
  DuplicateCheck :: State tx 'CheckDuplicate
  OutputCheck :: Coordinated.Runtime tx -> State tx 'CheckOutputs
  DepositCheck :: Coordinated.Runtime tx -> ~(UTxOType tx) -> State tx 'CheckDeposits
  Select :: Coordinated.Runtime tx -> State tx 'SelectSnapshot
  Retry :: Coordinated.Runtime tx -> ValidationError -> State tx 'RetryOrInvalidate
  Done :: State tx 'Finished

statePhase :: State tx phase -> Phase
statePhase = \case
  DuplicateCheck -> CheckDuplicate
  OutputCheck{} -> CheckOutputs
  DepositCheck{} -> CheckDeposits
  Select{} -> SelectSnapshot
  Retry{} -> RetryOrInvalidate
  Done -> Finished

machine :: forall tx. IsChainState tx => RuleEnvironment -> ChainPointTime -> OpenState tx -> TTL -> PendingDeposits tx -> tx -> Workflow tx ReqTxTopology
machine env ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx =
  workflow (show . statePhase) DuplicateCheck step
 where
  step :: State tx from -> Transition tx ReqTxTopology (State tx) from
  step = \case
    DuplicateCheck
      | SeenSnapshot snapshot _ _ <- seenSnapshot
      , Just{} <- snapshot.utxoToDecommit
      , Just known <- Map.lookup (txId tx) allTxs
      , matchesSnapshotDecommit snapshot known ->
          continue (const Done) $ Program.decide "transaction is retained by the candidate decommit" True
      | Map.member (txId tx) allTxs && any ((== txId tx) . txId) localTxs ->
          continue (const Done) $ Program.decide "transaction is already applied locally" True
      | otherwise -> continue OutputCheck $ do
          void $ Program.decide "transaction is already applied locally" False
          Coordinated.stage (Coordinated.start st) $ Coordinated.receiveTransaction tx
    OutputCheck runtime -> case checkTransactionOutputs localUTxO tx of
      Right () -> choose $ do
        void $ Program.decide "outputs do not collide with the local UTxO" True
        result <- Program.applyTransaction currentSlot localUTxO tx
        pure $ case result of
          Right after -> next $ DepositCheck runtime after
          Left err -> next $ Retry runtime err
      Left err -> continue (const $ Retry runtime err) $ Program.decide "outputs do not collide with the local UTxO" False
    DepositCheck runtime after
      | namesNoneOf (utxoFromTx tx) namedByDepositsInPlay -> continue Select $ do
          void $ Program.decide "outputs do not collide with pending deposits" True
          Coordinated.stage runtime $ Coordinated.applyTransaction tx localUTxO after
      | otherwise ->
          continue (const $ Retry runtime $ ValidationError "transaction creates an output a pending deposit names") $
            Program.decide "outputs do not collide with pending deposits" False
    Select runtime -> continue (const Done) $ do
      noSnapshot <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- Program.decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $
          case chainedSnapshotRequest env.snapshotLimits version previous (localTxs Seq.|> tx) pendingDeposits currentDepositTxId (decommitStatus version previous decommitTx) settlingCommit of
            Nothing -> void $ Program.decide "snapshot request has useful work" False
            Just request -> do
              void $ Program.decide "snapshot request has useful work" True
              sendSnapshot runtime request
    Retry runtime err -> continue (const Done) $ do
      -- Expiry still drops the payload. A later dependency does not wake this
      -- transaction; changing that recovery policy is deferred.
      canRetry <- Program.decide "retry budget remains" (ttl > 0)
      if canRetry
        then Program.suspend $ WaitOnNotApplicableTx err
        else void $ Coordinated.stage runtime $ Coordinated.forgetTransaction localUTxO tx err
    Done -> finish Done

  namedByDepositsInPlay :: UTxOType tx
  namedByDepositsInPlay =
    foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
      <> case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> deposited
        _ -> mempty
  OpenState{coordinatedHeadState, parameters} = st
  CoordinatedHeadState{localTxs, localUTxO, allTxs, confirmedSnapshot, seenSnapshot, decommitTx, version, currentDepositTxId} = coordinatedHeadState
  previous = getSnapshot confirmedSnapshot
  nextSn = previous.number + 1
  settlingCommit = settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous
