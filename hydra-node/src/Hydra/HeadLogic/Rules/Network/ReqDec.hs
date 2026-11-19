{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Receive a decommit request. Deposit selection takes precedence over a
-- pending decommit, and ledger applicability takes precedence over empty outputs.
module Hydra.HeadLogic.Rules.Network.ReqDec (Stage (..), ReqDecTopology, handle, workflow) where

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map (lookup)
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  change,
  decide,
  suspend,
 )
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Decommit qualified as Admission
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (depositPeriod, party, snapshotLimits),
 )
import Hydra.HeadLogic.Rules.SendSnapshot (broadcastSnapshot)
import Hydra.HeadLogic.Snapshot.Request (
  buildSnapshotRequest,
  decommitStatus,
  isLeader,
  requestDepositTxId,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    currentDepositTxId,
    localTxs,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  recordDecommit,
  start,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (
  run,
  workflow,
 )
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  PendingDeposits,
 )
import Hydra.Prelude hiding (State, handle, state)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data Stage = CheckCommit | ValidateDecommit | RecordAndRequest | Finished

type ReqDecTopology =
  'Topology
    '[ '( 'CheckCommit, '[ 'ValidateDecommit, 'Finished])
     , '( 'ValidateDecommit, '[ 'RecordAndRequest, 'Finished])
     , '( 'RecordAndRequest, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  CommitCheck :: State tx 'CheckCommit
  Validate :: State tx 'ValidateDecommit
  Record :: Admission.ValidatedDecommit tx -> State tx 'RecordAndRequest
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  CommitCheck -> CheckCommit
  Validate -> ValidateDecommit
  Record{} -> RecordAndRequest
  Done -> Finished

workflow :: forall tx. IsChainState tx => RuleEnvironment -> TTL -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> tx -> Workflow tx ReqDecTopology ()
workflow env ttl ChainPointTime{currentSlot, currentChainTime} pendingDeposits openState decommitTx =
  Workflow.workflow (show . statePhase) CommitCheck step
 where
  step :: State tx from -> Transition tx ReqDecTopology (State tx) from ()
  step = \case
    CommitCheck
      | Just depositTxId <- selectedDeposit
      , Just Deposit{deposited = commitUTxO} <- Map.lookup depositTxId pendingDeposits -> continue (const Done) $ do
          void $ Program.decide "next snapshot carries a deposit" True
          canRetry <- Program.decide "retry budget remains" (ttl > 0)
          if canRetry
            then Program.suspend WaitOnUnresolvedCommit{commitUTxO}
            else invalid DepositInFlight{depositTxId, commitUTxO}
      | otherwise -> continue (const Validate) $ Program.decide "next snapshot carries a deposit" False
    Validate -> choose $ do
      result <- Admission.validate env.snapshotLimits currentSlot coordinatedHeadState decommitTx
      case result of
        Right checked -> pure $ next $ Record checked
        Left (Admission.NotApplicable reason) -> notApplicable reason $> next Done
        Left (Admission.Invalid reason) -> invalid reason $> next Done
    Record checked -> continue (const Done) $ do
      void $ Staging.stage (Coordinated.start openState) $ Coordinated.recordDecommit checked
      noSnapshot <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- Program.decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $ do
          void $ Program.decide "snapshot request has useful work" (maybe False snd selectedRequest)
          forM_ selectedRequest $ \(request, useful) -> when useful $ broadcastSnapshot request
    Done -> finish Done ()

  invalid :: DecommitInvalidReason tx -> Program tx ()
  invalid reason = Program.change DecommitInvalid{headId, decommitTx, decommitInvalidReason = reason}
  notApplicable :: DecommitInvalidReason tx -> Program tx ()
  notApplicable reason = do
    canRetry <- Program.decide "retry budget remains" (ttl > 0)
    if canRetry then Program.suspend $ WaitOnNotApplicableDecommitTx reason else invalid reason
  selectedDeposit = selectedRequest >>= requestDepositTxId . fst
  selectedRequest =
    buildSnapshotRequest env.snapshotLimits version previous localTxs pendingDeposits currentDepositTxId (decommitStatus version previous $ Just decommitTx) (settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous)
  previous = getSnapshot confirmedSnapshot
  nextSn = previous.number + 1
  CoordinatedHeadState{confirmedSnapshot, localTxs, version, seenSnapshot, currentDepositTxId} = coordinatedHeadState
  OpenState{headId, parameters, coordinatedHeadState} = openState

-- | Handle one input through the inspectable workflow.
handle :: forall tx. IsChainState tx => RuleEnvironment -> TTL -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> tx -> Program tx ()
handle env ttl chainPoint pendingDeposits openState decommitTx = Workflow.run $ workflow env ttl chainPoint pendingDeposits openState decommitTx
