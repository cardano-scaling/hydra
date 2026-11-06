{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Validate a snapshot request, sign it and prune local transactions. A phase
-- carries only the values established by the preceding program action.
module Hydra.HeadLogic.Rules.ReqSn (Phase (..), ReqSnTopology, reqSnTopology, sReqSnTopology, machine) where

import Crem.BaseMachine (BaseMachineT (..), InitialState (..))
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (LogicError (..), RequirementFailure (..))
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (..), WaitReason (..))
import Hydra.HeadLogic.Program (SnapshotProgram)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (depositPeriod),
 )
import Hydra.HeadLogic.Rules.SnapshotRequest (
  SettlingCommit (ContinueCommit),
  isLeader,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.Rules.Transactions (applyTransactionsProgram, pruneTransactions)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    finalizedCommit,
    localTxs,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  seenSnapshotNumber,
 )
import Hydra.HeadLogic.State.Operations (
  absorbable,
  finalizedDepositTxId,
  settledUTxO,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), PendingDeposits)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (TxIdType, UTxOType, combinedUTxO, txId, utxoFromTx, withoutUTxO)
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

$( singletons
    [d|
      data Phase = CheckRequest | ResolveDecommit | ResolveDeposit | ResolveTransactions | ApplyTransactions | BuildSnapshot | SignAndAcknowledge | PruneLocalTransactions | Finished

      reqSnTopology :: Topology Phase
      reqSnTopology =
        Topology
          [ (CheckRequest, [ResolveDecommit])
          , (ResolveDecommit, [ResolveDeposit])
          , (ResolveDeposit, [ResolveTransactions])
          , (ResolveTransactions, [ApplyTransactions])
          , (ApplyTransactions, [BuildSnapshot])
          , (BuildSnapshot, [SignAndAcknowledge])
          , (SignAndAcknowledge, [PruneLocalTransactions])
          , (PruneLocalTransactions, [Finished])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data State tx phase where
  Request :: State tx 'CheckRequest
  Checked :: State tx 'ResolveDecommit
  DecommitResolved :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> State tx 'ResolveDeposit
  DepositResolved :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> State tx 'ResolveTransactions
  TransactionsResolved :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> [tx] -> State tx 'ApplyTransactions
  TransactionsApplied :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> [tx] -> State tx 'BuildSnapshot
  Built :: Snapshot tx -> State tx 'SignAndAcknowledge
  Acknowledged :: Snapshot tx -> State tx 'PruneLocalTransactions
  Done :: State tx 'Finished

machine :: forall tx. IsChainState tx => RuleEnvironment -> PendingDeposits tx -> ChainPointTime -> OpenState tx -> TTL -> Party -> SnapshotVersion -> SnapshotNumber -> [TxIdType tx] -> Maybe tx -> Maybe (TxIdType tx) -> Workflow tx 'True 'True ReqSnTopology
machine env pendingDeposits ChainPointTime{currentSlot, currentChainTime} st ttl otherParty sv sn requestedTxIds mDecommitTx mDepositTxId =
  BaseMachineT (InitialState Request) $ \state () -> step state
 where
  step :: State tx from -> Transition tx 'True 'True ReqSnTopology (State tx) from
  step = \case
    Request -> continue (const Checked) $ do
      validNumber <- Program.decide "request is the next snapshot" (sn == seenSn + 1)
      unless validNumber $ Program.reject $ RequireFailed ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
      leader <- Program.decide "sender leads this snapshot" (isLeader parameters otherParty sn)
      unless leader $ Program.reject $ RequireFailed ReqSnNotLeader{requestedSn = sn, leader = otherParty}
      confirmed <- Program.decide "previous snapshot is confirmed" (confSn == seenSn)
      unless confirmed $ Program.suspend $ WaitOnSnapshotNumber seenSn
      -- A follower may still be waiting for the increment/decrement observation.
      matches <- Program.decide "snapshot version matches" (version == sv)
      unless matches $ Program.suspend $ WaitOnSnapshotVersion sv
    Checked -> continue (uncurry DecommitResolved) resolveDecommit
    DecommitResolved active decommit -> continue (\commit -> DepositResolved active commit decommit) resolveDeposit
    DepositResolved active commit decommit -> continue (TransactionsResolved active commit decommit) $
      case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
        [] -> pure $ mapMaybe (`Map.lookup` allTxs) requestedTxIds
        unseen -> Program.suspend $ WaitOnTxs unseen
    TransactionsResolved active commit decommit txs -> continue (\after -> TransactionsApplied after commit decommit txs) $ do
      case (mDepositTxId, commit) of
        (Just depositTxId, Just deposited)
          | not (absorbable active txs deposited) ->
              Program.reject $ RequireFailed ReqSnDepositOutputsHeld{depositTxId}
        _ -> pure ()
      -- Registry entries may be unvalidated receipts. Apply every requested tx.
      result <- applyTransactionsProgram currentSlot active txs
      case result of
        Left (tx, err) -> Program.reject $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
        Right after -> pure after
    TransactionsApplied after commit decommit txs -> continue Built $ do
      let accumulator =
            Accumulator.applyUTxODelta
              previous.accumulator
              (combinedUTxO previous.utxo Nothing previous.utxoToDecommit)
              (combinedUTxO after Nothing decommit)
          appliedAccumulator
            | isNothing commit && isNothing decommit = accumulator
            | otherwise =
                Accumulator.applyUTxODelta
                  previous.appliedAccumulator
                  (combinedUTxO previous.utxo previous.utxoToCommit Nothing)
                  (combinedUTxO after commit Nothing)
      checkSize "unsettled fanout fits accumulator capacity" accumulator
      checkSize "settled fanout fits accumulator capacity" appliedAccumulator
      pure Snapshot{headId, version, number = sn, confirmed = txs, utxo = after, utxoToCommit = commit, utxoToDecommit = decommit, depositTxId = mDepositTxId, accumulator, appliedAccumulator}
    Built snapshot -> continue (const $ Acknowledged snapshot) $ do
      signature <- Program.signSnapshot snapshot
      Program.emit $ NetworkEffect $ AckSn signature sn
    Acknowledged snapshot -> continue (const Done) $ do
      -- The acknowledgement is buffered before pruning; SnapshotRequested then
      -- reconstructs localUTxO by applying the retained transactions in order.
      newLocalTxs <- pruneTransactions currentSlot snapshot.utxoToCommit snapshot.utxoToDecommit snapshot.utxo localTxs
      void $ Coordinated.stage (Coordinated.start st) $ Coordinated.acceptSnapshot snapshot newLocalTxs mDepositTxId
    Done -> finish Done

  resolveDecommit :: SnapshotProgram tx (UTxOType tx, Maybe (UTxOType tx))
  resolveDecommit = case mDecommitTx of
    Nothing
      | sv == confVersion && isJust confUTxOToDecommit -> Program.reject $ RequireFailed ReqSnDecommitNotSettled
      | otherwise -> pure (confirmedUTxO, Nothing)
    Just tx
      | Just depositTxId <- mDepositTxId -> Program.reject $ RequireFailed ReqSnBothCommitAndDecommit{depositTxId, decommitTxId = txId tx}
      | utxoFromTx tx == mempty -> Program.reject $ RequireFailed ReqSnDecommitNoOutputs{decommitTxId = txId tx}
      | sv == confVersion && isJust confUTxOToDecommit ->
          if confUTxOToDecommit == Just (utxoFromTx tx)
            then pure (confirmedUTxO, confUTxOToDecommit)
            else Program.reject $ RequireFailed ReqSnDecommitNotSettled
      | otherwise -> do
          result <- Program.applyTransaction currentSlot confirmedUTxO tx
          case result of
            Left err -> Program.reject $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
            Right after -> let outputs = utxoFromTx tx in pure (after `withoutUTxO` outputs, Just outputs)

  -- A deposit is carried separately: it becomes spendable only after increment.
  resolveDeposit :: SnapshotProgram tx (Maybe (UTxOType tx))
  resolveDeposit = case mDepositTxId of
    Nothing -> case settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous of
      Just (ContinueCommit (_, Deposit{deposited})) -> Program.suspend WaitOnUnresolvedCommit{commitUTxO = deposited}
      _ -> pure Nothing
    Just depositTxId
      | finalizedDepositTxId finalizedCommit == Just depositTxId -> Program.reject $ RequireFailed ReqSnDepositBlockedByFinalizedCommit{depositTxId}
      | otherwise -> case Map.lookup depositTxId pendingDeposits of
          Nothing
            | ttl > 0 -> Program.suspend WaitOnDepositObserved{depositTxId}
            | otherwise -> Program.reject $ RequireFailed RequestedDepositNotFoundLocally{depositTxId}
          Just Deposit{status, deposited}
            | status == Inactive -> Program.suspend WaitOnDepositActivation{depositTxId}
            -- Identity matters as well as content. An expired deposit remains
            -- admissible here while its already-signed increment can still land.
            | sv == confVersion && isJust confUTxOToCommit ->
                if confUTxOToCommit == Just deposited && confDepositTxId == Just depositTxId
                  then pure confUTxOToCommit
                  else Program.reject $ RequireFailed ReqSnCommitNotSettled
            | status == Expired -> Program.reject $ RequireFailed RequestedDepositExpired{depositTxId}
            | otherwise -> pure $ Just deposited

  checkSize :: Text -> Accumulator.HydraAccumulator -> SnapshotProgram tx ()
  checkSize decision accumulator = do
    let result = Accumulator.checkAccumulatorSize accumulator
    void $ Program.decide decision (isRight result)
    case result of
      Left AccumulatorTooLarge{utxoCount, maxAllowed} -> Program.reject $ RequireFailed ReqSnUTxOSetTooLarge{utxoCount, maxAllowed}
      Right () -> pure ()

  previous = getSnapshot confirmedSnapshot
  confSn = case confirmedSnapshot of InitialSnapshot{} -> 0; ConfirmedSnapshot{} -> previous.number
  confVersion = previous.version
  confUTxOToCommit = case confirmedSnapshot of InitialSnapshot{} -> Nothing; ConfirmedSnapshot{} -> previous.utxoToCommit
  confDepositTxId = case confirmedSnapshot of InitialSnapshot{} -> Nothing; ConfirmedSnapshot{} -> previous.depositTxId
  confUTxOToDecommit = case confirmedSnapshot of InitialSnapshot{} -> Nothing; ConfirmedSnapshot{} -> previous.utxoToDecommit
  seenSn = seenSnapshotNumber seenSnapshot
  confirmedUTxO = settledUTxO version previous
  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs, version, finalizedCommit} = coordinatedHeadState
  OpenState{parameters, coordinatedHeadState, headId} = st
