{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Head phases observed or recorded by the node. Pending fanout is optimistic:
-- a selection is recorded, but no distributed output has been observed yet.
module Hydra.HeadLogic.Machine.Lifecycle (Phase (..), LifecycleTopology, lifecycleTopology, sLifecycleTopology, phase, machine) where

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), pureResult)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology
import Data.Singletons.Base.TH
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Machine.Types (Transition (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State qualified as Head
import Hydra.HeadLogic.State.Operations (closedToFanoutProgress, fanoutProgressToClosed, nullOutputs, recordedSelectionMode)
import Hydra.Node.State (NodeState (..))
import Hydra.Prelude
import Hydra.Tx (IsTx (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))

$( singletons
    [d|
      data Phase = Idle | Open | Closed | ReadyToFanout | PendingFanout | FanoutProgress

      lifecycleTopology :: Topology Phase
      lifecycleTopology =
        Topology
          [ (Idle, [Open])
          , (Open, [Closed])
          , (Closed, [Open, ReadyToFanout, PendingFanout, FanoutProgress, Idle])
          , (ReadyToFanout, [Open, PendingFanout, FanoutProgress, Idle])
          , (PendingFanout, [Open, ReadyToFanout, FanoutProgress, Idle])
          , (FanoutProgress, [Open, ReadyToFanout, PendingFanout, Idle])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase

deriving via AllVertices Phase instance RenderableVertices Phase

-- | ReadyToFanout records the notification flag, not the current clock guard.
-- Event replay permits HeadOpened to replace any phase, hence the Open edges.
phase :: IsTx tx => Head.HeadState tx -> Phase
phase = \case
  Head.Idle{} -> Idle
  Head.Open{} -> Open
  Head.Closed Head.ClosedState{readyToFanoutSent} -> if readyToFanoutSent then ReadyToFanout else Closed
  Head.FanoutProgress Head.PartialFanoutState{distributedOutputs} ->
    if null (outputsOfUTxO distributedOutputs) then PendingFanout else FanoutProgress

-- | The phase index is local to this component; payloads live in the shared
-- NodeState. Other components preserve the outer head constructor and fanout
-- fields, so the continuation remains aligned with that state.
machine :: forall tx m. (IsChainState tx, Applicative m) => Head.HeadState tx -> BaseMachineT m LifecycleTopology (Transition tx) (Transition tx)
machine initial = BaseMachineT{initialState, action}
 where
  initialState :: InitialState SPhase
  initialState = case phase initial of
    Idle -> InitialState SIdle
    Open -> InitialState SOpen
    Closed -> InitialState SClosed
    ReadyToFanout -> InitialState SReadyToFanout
    PendingFanout -> InitialState SPendingFanout
    FanoutProgress -> InitialState SFanoutProgress

  action :: forall from. SPhase from -> Transition tx -> ActionResult m LifecycleTopology SPhase from (Transition tx)
  action current input = case input.event of
    ChainRolledBack{chainState} -> advance input current $ Head.setChainState chainState
    _ -> case current of
      SIdle -> idle input
      SOpen -> open input
      SClosed -> closed SClosed input
      SReadyToFanout -> closed SReadyToFanout input
      SPendingFanout -> fanout SPendingFanout input
      SFanoutProgress -> fanout SFanoutProgress input

idle :: (IsChainState tx, Applicative m) => Transition tx -> ActionResult m LifecycleTopology SPhase 'Idle (Transition tx)
idle input = case input.event of
  HeadOpened{} -> openHead input
  _ -> pureResult input SIdle

open :: (IsChainState tx, Applicative m) => Transition tx -> ActionResult m LifecycleTopology SPhase 'Open (Transition tx)
open input = case input.event of
  HeadOpened{} -> openHead input
  HeadClosed{chainState, contestationDeadline} -> advance input SClosed $ \case
    Head.Open Head.OpenState{parameters, coordinatedHeadState, headId, headSeed} ->
      Head.Closed
        Head.ClosedState
          { parameters
          , confirmedSnapshot = coordinatedHeadState.confirmedSnapshot
          , contestationDeadline
          , readyToFanoutSent = False
          , chainState
          , headId
          , headSeed
          , version = coordinatedHeadState.version
          , unconfirmedSnapshot = case coordinatedHeadState.seenSnapshot of
              Head.SeenSnapshot{snapshot} -> Just snapshot
              _ -> Nothing
          }
    other -> other
  _ -> pureResult input SOpen

closed ::
  ( IsChainState tx
  , Applicative m
  , AllowedTransition LifecycleTopology from 'Open
  , AllowedTransition LifecycleTopology from 'Idle
  , AllowedTransition LifecycleTopology from 'ReadyToFanout
  , AllowedTransition LifecycleTopology from 'PendingFanout
  , AllowedTransition LifecycleTopology from 'FanoutProgress
  ) =>
  SPhase from ->
  Transition tx ->
  ActionResult m LifecycleTopology SPhase from (Transition tx)
closed current input = case input.event of
  HeadOpened{} -> openHead input
  HeadContested{chainState, contestationDeadline} -> advance input current $ \case
    Head.Closed st -> Head.Closed st{Head.chainState, Head.contestationDeadline}
    other -> other
  HeadIsReadyToFanout{} -> advance input SReadyToFanout $ \case
    Head.Closed st -> Head.Closed st{Head.readyToFanoutSent = True}
    other -> other
  HeadFannedOut{chainState} -> advance input SIdle $ const $ Head.Idle Head.IdleState{chainState}
  HeadFanoutInitiated{remainingOutputs} -> advance input SPendingFanout $ \case
    Head.Closed st -> closedToFanoutProgress st st.chainState remainingOutputs mempty Head.AutoDrain
    other -> other
  HeadPartialFanoutSelected{remainingOutputs, selection} -> advance input SPendingFanout $ \case
    Head.Closed st -> closedToFanoutProgress st st.chainState remainingOutputs mempty $ recordedSelectionMode mempty remainingOutputs selection
    other -> other
  HeadPartialFannedOut{distributedOutputs, remainingOutputs, chainState, mode} ->
    let update = \case
          Head.Closed st -> closedToFanoutProgress st chainState remainingOutputs distributedOutputs mode
          other -> other
     in if nullOutputs distributedOutputs
          then advance input SPendingFanout update
          else advance input SFanoutProgress update
  _ -> pureResult input current

fanout ::
  ( IsChainState tx
  , Applicative m
  , AllowedTransition LifecycleTopology from 'Open
  , AllowedTransition LifecycleTopology from 'Idle
  , AllowedTransition LifecycleTopology from 'ReadyToFanout
  , AllowedTransition LifecycleTopology from 'PendingFanout
  , AllowedTransition LifecycleTopology from 'FanoutProgress
  ) =>
  SPhase from ->
  Transition tx ->
  ActionResult m LifecycleTopology SPhase from (Transition tx)
fanout current input = case input.event of
  HeadOpened{} -> openHead input
  HeadFannedOut{chainState} -> advance input SIdle $ const $ Head.Idle Head.IdleState{chainState}
  HeadFanoutReverted{} -> advance input SReadyToFanout $ \case
    Head.FanoutProgress st -> Head.Closed $ fanoutProgressToClosed st
    other -> other
  HeadFanoutInitiated{remainingOutputs} -> advance input current $ \case
    Head.FanoutProgress st -> Head.FanoutProgress st{Head.mode = Head.AutoDrain, Head.remainingOutputs}
    other -> other
  HeadPartialFanoutSelected{remainingOutputs, selection} -> advance input current $ \case
    Head.FanoutProgress st -> Head.FanoutProgress st{Head.mode = recordedSelectionMode st.distributedOutputs remainingOutputs selection}
    other -> other
  HeadPartialFannedOut{distributedOutputs = newlyDistributed, remainingOutputs, chainState, mode} ->
    let prior = case input.before.headState of
          Head.FanoutProgress st -> st.distributedOutputs
          _ -> mempty
        distributedOutputs = prior <> newlyDistributed
        update = \case
          Head.FanoutProgress st -> Head.FanoutProgress st{Head.chainState, Head.remainingOutputs, Head.distributedOutputs = distributedOutputs, Head.mode}
          other -> other
     in if nullOutputs distributedOutputs
          then advance input SPendingFanout update
          else advance input SFanoutProgress update
  _ -> pureResult input current

openHead ::
  (IsChainState tx, Applicative m, AllowedTransition LifecycleTopology from 'Open) =>
  Transition tx ->
  ActionResult m LifecycleTopology SPhase from (Transition tx)
openHead input = advance input SOpen $ \old -> case input.event of
  HeadOpened{headSeed, headId, parameters, chainState} ->
    Head.Open
      Head.OpenState
        { headId
        , headSeed
        , parameters
        , chainState
        , coordinatedHeadState =
            Head.CoordinatedHeadState
              { localUTxO = mempty
              , allTxs = mempty
              , localTxs = mempty
              , confirmedSnapshot = InitialSnapshot{headId}
              , seenSnapshot = Head.NoSeenSnapshot
              , currentDepositTxId = Nothing
              , decommitTx = Nothing
              , version = 0
              , finalizedCommit = Nothing
              , finalizedDecommit = Nothing
              }
        }
  _ -> old

advance ::
  (Applicative m, AllowedTransition LifecycleTopology from to) =>
  Transition tx ->
  SPhase to ->
  (Head.HeadState tx -> Head.HeadState tx) ->
  ActionResult m LifecycleTopology SPhase from (Transition tx)
advance input next update =
  pureResult input{current = input.current{headState = update input.current.headState}} next
