{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain where

import Hydra.API.ServerOutput qualified as ServerOutput (
  ClientMessage (PostTxOnChainFailed, postChainTx, postTxError),
 )
import Hydra.Chain (
  ChainEvent (
    Observation,
    PostTxError,
    Rollback,
    Tick,
    chainPoint,
    chainTime,
    newChainState,
    observedTx,
    postChainTx,
    postTxError,
    rolledBackChainState
  ),
  OnChainTx (
    OnCloseTx,
    OnContestTx,
    OnDecrementTx,
    OnDepositTx,
    OnFanoutTx,
    OnIncrementTx,
    OnInitTx,
    OnPartialFanoutTx,
    OnRecoverTx,
    contestationDeadline,
    depositTxId,
    distributedOutputs,
    distributedUTxO,
    fanoutUTxO,
    headId,
    headParameters,
    headSeed,
    newVersion,
    participants,
    signatures,
    snapshotNumber
  ),
  PostTxError (StalePartialFanoutTx),
 )
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Error (LogicError (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (Effect (ClientEffect))
import Hydra.HeadLogic.Program (Program, emit, reject)
import Hydra.HeadLogic.Rules.Chain.Deposits (handleDepositObservation)
import Hydra.HeadLogic.Rules.Chain.Fanout (
  onClosedChainFanoutTx,
  onClosedChainPartialFanoutTx,
  onFanoutPostTxError,
  onPartialFanoutChainFanoutTx,
  onPartialFanoutChainPartialFanoutTx,
 )
import Hydra.HeadLogic.Rules.Chain.Lifecycle (
  onClosedChainContestTx,
  onIdleChainInitTx,
  onOpenChainCloseTx,
 )
import Hydra.HeadLogic.Rules.Chain.Rollback (handleRollback)
import Hydra.HeadLogic.Rules.Chain.Settlement (
  Settlement (Decrement, Increment),
  handleSettlement,
 )
import Hydra.HeadLogic.Rules.Chain.Tick (handleTick)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.State (
  ClosedState (ClosedState, headId),
  HeadState (..),
  OpenState (OpenState, headId),
  PartialFanoutState (PartialFanoutState, headId),
  canOpenHead,
 )
import Hydra.HeadLogic.State.Fanout (getClosedState)
import Hydra.Node.State (PendingDeposits, SyncedStatus, depositsForHeadWithoutFinalizedCommit)
import Hydra.Prelude

handleChainInput ::
  IsChainState tx =>
  RuleEnvironment ->
  -- | Current system time.
  UTCTime ->
  PendingDeposits tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  SyncedStatus ->
  Program tx ()
handleChainInput env now pendingDeposits st ev syncStatus = case (st, ev) of
  (_, ChainInput Tick{chainTime, chainPoint}) ->
    handleTick env now pendingDeposits st chainTime chainPoint syncStatus
  (_, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    handleRollback env now pendingDeposits st rolledBackChainState chainTime syncStatus
  (_, ChainInput Observation{observedTx = OnInitTx{headId, headSeed, headParameters, participants}, newChainState})
    | canOpenHead st headId ->
        onIdleChainInitTx env newChainState headId headSeed headParameters participants
  -- Open
  ( Open openState@OpenState{headId = ourHeadId}
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline, signatures}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline signatures
      | otherwise ->
          reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnIncrementTx{headId, newVersion, depositTxId, snapshotNumber, signatures}, newChainState})
    | ourHeadId == headId ->
        handleSettlement env (depositsForHeadWithoutFinalizedCommit openState pendingDeposits) openState newChainState newVersion (Increment depositTxId) snapshotNumber signatures
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO, snapshotNumber, signatures}, newChainState})
    | ourHeadId == headId ->
        handleSettlement env (depositsForHeadWithoutFinalizedCommit openState pendingDeposits) openState newChainState newVersion (Decrement distributedUTxO) snapshotNumber signatures
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (_, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline, signatures}, newChainState})
    | Just closedState@ClosedState{headId = ourHeadId} <- getClosedState st ->
        if ourHeadId == headId
          then onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline signatures
          else reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onClosedChainFanoutTx closedState newChainState fanoutUTxO
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onClosedChainPartialFanoutTx closedState newChainState distributedOutputs
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainPartialFanoutTx partialFanoutState newChainState distributedOutputs
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainFanoutTx partialFanoutState newChainState fanoutUTxO
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  (_, ChainInput Observation{observedTx = OnDepositTx{}}) ->
    handleDepositObservation pendingDeposits st ev
  (_, ChainInput Observation{observedTx = OnRecoverTx{}}) ->
    handleDepositObservation pendingDeposits st ev
  (_, ChainInput PostTxError{postTxError = StalePartialFanoutTx}) ->
    -- The chain advanced past this step before we could post it (another node
    -- was faster). The chain observation loop already emitted the correct next
    -- step, so this is safe to ignore.
    pure ()
  (FanoutProgress pfs, ChainInput PostTxError{postChainTx, postTxError}) ->
    onFanoutPostTxError pfs postChainTx postTxError
  (_, ChainInput PostTxError{postChainTx, postTxError}) ->
    emit . ClientEffect $ ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
  _ ->
    reject $ UnhandledInput ev st
