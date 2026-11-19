{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Pure input programs and retained-event aggregation. Runtime entry points
-- and Cardano configuration live in Hydra.Node.HeadLogic.
module Hydra.HeadLogic (
  module Hydra.HeadLogic,
  aggregateChainStateHistory,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Snapshot.Settlement,
  module Hydra.HeadLogic.State.Fanout,
  module Hydra.HeadLogic.Fanout.Outputs,
  module Hydra.HeadLogic.Transaction.Outputs,
  module Hydra.HeadLogic.Outcome,
  module Hydra.HeadLogic.Rules.Environment,
  module Hydra.HeadLogic.Snapshot.Request,
  module Hydra.HeadLogic.Rules.Transactions,
  module Hydra.HeadLogic.Fanout.Selection,
  openFinalizedDeposit,
) where

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Fanout.Outputs (isSubMultisetOf, nullOutputs, removeDistributedOutputs, sameOutputs)
import Hydra.HeadLogic.Fanout.Selection (NextFanoutStep (..), computeFullFanoutUTxO, currentFanoutStep, effectiveCommitDecommit, fanoutUTxOFromSnapshot, matchesFanoutStep, mkFullFanoutTx, nextFanoutStep, recordedSelectionMode)
import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.HeadLogic.Outcome (Effect (..), Outcome (..), StateChanged (..), WaitReason (..), cause, eventHeadId, newState, noop)
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Rules.Client (openFinalizedDeposit)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Rules.Input (handleInput)
import Hydra.HeadLogic.Rules.Transactions (applyTransactionsProgram, applyTransactionsWithoutCollision, checkTransactionOutputs)
import Hydra.HeadLogic.Snapshot.Request (SettlingCommit (..), SnapshotRequest, buildSnapshotRequest, chainedSnapshotRequest, existingDeposit, isLeader, maxTxsPerSnapshot, nextActiveDepositId, nextSnapshotRequest, requestDecommitTx, requestDepositTxId, requestTxs, scopedDeposits, selectNextDeposit, settlingCommitAtPeriod)
import Hydra.HeadLogic.Snapshot.Settlement (finalizedDepositTxId, retainFinalizedCommit, retainFinalizedDecommit, settledUTxO)
import Hydra.HeadLogic.State (ClosedState (..), CoordinatedHeadState (..), FanoutMode (..), FinalizedSnapshot (..), FinalizedState (..), HeadState (..), IdleState (..), OnChainFanoutDatum (..), OpenState (..), PartialFanoutState (..), SeenSnapshot (..), closedStateCBORTag, closedStateCBORTagV1, coordinatedHeadStateCBORTag, coordinatedHeadStateCBORTagV1, getChainState, getHeadParameters, getOpenStateConfirmedSnapshot, headIdOf, isCollectingAcks, mkSeenSnapshot, seenSnapshotNumber, snapshotInFlight)
import Hydra.HeadLogic.State.ChainHistory (aggregateChainStateHistory)
import Hydra.HeadLogic.State.Fanout (fanoutProgressToClosed)
import Hydra.HeadLogic.State.Node qualified as Machine
import Hydra.HeadLogic.Transaction.Outputs (absorbable, namesNoneOf)
import Hydra.Node.State (NodeState (..))
import Hydra.Prelude

updateProgram :: IsChainState tx => RuleEnvironment -> UTCTime -> NodeState tx -> Input tx -> Program tx ()
updateProgram = handleInput

aggregateNodeState :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
aggregateNodeState = Machine.apply

aggregateState ::
  IsChainState tx =>
  NodeState tx ->
  Outcome tx ->
  NodeState tx
aggregateState s outcome =
  Machine.applyEvents s $ collectStateChanged outcome
 where
  collectStateChanged :: Outcome tx -> [StateChanged tx]
  collectStateChanged = \case
    Error{} -> []
    Wait{stateChanges} -> stateChanges
    Continue{stateChanges} -> stateChanges
