{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain where

import Data.Map.Strict qualified as Map
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (ChainEvent (..), OnChainTx (..), PostChainTx (..), PostTxError (..))
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Error (LogicError (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, emit, reject)
import Hydra.HeadLogic.Rules.Close (
  onClosedChainContestTx,
  onOpenChainCloseTx,
 )
import Hydra.HeadLogic.Rules.Deposits (handleTick)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Rules.Fanout (
  onClosedChainFanoutTx,
  onClosedChainPartialFanoutTx,
  onPartialFanoutChainFanoutTx,
  onPartialFanoutChainPartialFanoutTx,
 )
import Hydra.HeadLogic.Rules.Fanout.Selection (
  currentFanoutStep,
  matchesFanoutStep,
 )
import Hydra.HeadLogic.Rules.Initialise (onIdleChainInitTx)
import Hydra.HeadLogic.Rules.Repost (handleRollback)
import Hydra.HeadLogic.Rules.Settlement qualified as Settlement
import Hydra.HeadLogic.Rules.SnapshotRequest (scopedDeposits)
import Hydra.HeadLogic.State (
  ClosedState (ClosedState, headId),
  CoordinatedHeadState (CoordinatedHeadState, finalizedCommit),
  FinalizedSnapshot (FinalizedSnapshot, snapshot),
  HeadState (..),
  OpenState (
    OpenState,
    coordinatedHeadState,
    headId,
    headSeed,
    parameters
  ),
  PartialFanoutState (
    PartialFanoutState,
    distributedOutputs,
    headId
  ),
 )
import Hydra.HeadLogic.State.Operations (
  finalizedDepositTxId,
  nullOutputs,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Node.State (PendingDeposits, SyncedStatus)
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
  (Idle _, ChainInput Observation{observedTx = OnInitTx{headId, headSeed, headParameters, participants}, newChainState}) ->
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
        Workflow.run $ Settlement.machine env (scopedDeposits openState pendingDeposits) openState newChainState newVersion (Settlement.Increment depositTxId) snapshotNumber signatures
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO, snapshotNumber, signatures}, newChainState})
    | ourHeadId == headId ->
        Workflow.run $ Settlement.machine env (scopedDeposits openState pendingDeposits) openState newChainState newVersion (Settlement.Decrement distributedUTxO) snapshotNumber signatures
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline, signatures}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline signatures
    | otherwise ->
        reject NotOurHead{ourHeadId, otherHeadId = headId}
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
  -- Node-level: deposit/recover observations scoped to our head
  ( Open OpenState{headId = ourHeadId, headSeed, parameters, coordinatedHeadState = CoordinatedHeadState{finalizedCommit}}
    , ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState}
    )
      | ourHeadId == headId ->
          change DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
            -- The finalized deposit observed *again* means a rollback erased
            -- both the deposit and its finalized increment, and the deposit tx
            -- just re-landed on the new chain. The increment re-post issued at
            -- rollback time necessarily failed (the deposit UTxO did not exist
            -- on the new chain yet) and is not retried anywhere else, while
            -- every alternative settlement of this deposit is deliberately
            -- blocked (see 'finalizedDepositTxId'): re-post the increment
            -- now that the deposit is on chain again, see #2741.
            *> case finalizedCommit of
              Just FinalizedSnapshot{snapshot}
                | finalizedDepositTxId finalizedCommit == Just depositTxId ->
                    emit
                      OnChainEffect
                        { postChainTx =
                            IncrementTx
                              { headSeed
                              , headId = ourHeadId
                              , headParameters = parameters
                              , incrementingSnapshot = snapshot
                              , depositTxId
                              }
                        }
              _ -> pure ()
      | otherwise ->
          pure ()
  (Closed ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        change DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        pure ()
  -- Mirror the 'Closed' case while mid-fanout: a deposit observed during a
  -- (partial) fanout must still be recorded so it remains recoverable via
  -- 'Recover'. Without this the input falls through to 'Error' and is dropped.
  (FanoutProgress PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        change DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        pure ()
  (Idle _, ChainInput Observation{observedTx = OnDepositTx{}}) ->
    pure ()
  -- Deposit recovery is node-level: emit DepositRecovered for any tracked deposit
  -- regardless of which head is currently active. Previous-head deposits survive
  -- fanout in 'pendingDeposits', so recovery works even while a new head is Open.
  -- Unrelated deposits (never in pendingDeposits) are silently ignored.
  (_, ChainInput Observation{observedTx = OnRecoverTx{headId, recoveredTxId, recoveredUTxO}, newChainState})
    | Map.member recoveredTxId pendingDeposits ->
        change DepositRecovered{chainState = newChainState, headId, depositTxId = recoveredTxId, recovered = recoveredUTxO}
    | otherwise ->
        pure ()
  (_, ChainInput PostTxError{postTxError = StalePartialFanoutTx}) ->
    -- The chain advanced past this step before we could post it (another node
    -- was faster). The chain observation loop already emitted the correct next
    -- step, so this is safe to ignore.
    pure ()
  (FanoutProgress pfs, ChainInput PostTxError{postChainTx, postTxError})
    -- We optimistically moved 'Closed' → 'PartialFanout' when the fanout was
    -- initiated. If posting the initiating fanout tx fails terminally before
    -- anything has been distributed on chain (so the on-chain datum is still
    -- 'Closed'), revert to 'Closed' rather than wedging the head — otherwise
    -- 'Fanout' stays rejected and there is no clean way to recover. Once any
    -- partial fanout has landed ('distributedOutputs' non-empty) the on-chain
    -- datum is genuinely 'FanoutProgress', so we must not revert.
    --
    -- Only for a failure of the step this node is actually driving, of which
    -- 'AwaitingSelection' has none. A superseded transaction can still be in
    -- flight — a selection covering the whole remainder turns into a full fanout
    -- and leaves the earlier chunk behind — and reverting on its failure would
    -- drop the driver role for a transaction nobody is waiting on.
    | PartialFanoutState{headId, distributedOutputs} <- pfs
    , nullOutputs distributedOutputs
    , maybe False (`matchesFanoutStep` postChainTx) (currentFanoutStep pfs) ->
        change HeadFanoutReverted{headId}
          *> emit (ClientEffect ServerOutput.PostTxOnChainFailed{postChainTx, postTxError})
  (_, ChainInput PostTxError{postChainTx, postTxError}) ->
    emit . ClientEffect $ ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
  _ ->
    reject $ UnhandledInput ev st
