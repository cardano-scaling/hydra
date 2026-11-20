{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Lifecycle of one Head. Local requests do not change its protocol phase.
-- Finalization is terminal; the node creates a new machine for another Head.
module Hydra.HeadLogic.State.Head (Phase (..), LifecycleTopology, lifecycleTopology, sLifecycleTopology, transitionLabels, machine) where

import Crem.BaseMachine (
  ActionResult,
  BaseMachineT (..),
  InitialState (..),
  pureResult,
  withStateInspector,
 )
import Crem.Render.RenderableVertices (
  AllVertices (..),
  RenderableVertices,
 )
import Crem.StateInspection (describeOnly)
import Crem.Topology (
  AllowedTransition,
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Fanout.Selection (recordedSelectionMode)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State qualified as Head (
  ClosedState (..),
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    finalizedCommit,
    finalizedDecommit,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  FanoutMode (AutoDrain),
  FinalizedState (..),
  HeadState (..),
  IdleState (..),
  OnChainFanoutDatum (..),
  OpenState (
    OpenState,
    chainState,
    coordinatedHeadState,
    headId,
    headSeed,
    parameters
  ),
  PartialFanoutState (
    chainState,
    distributedOutputs,
    fanoutDatum,
    headId,
    mode,
    readyToFanoutSent,
    remainingOutputs
  ),
  SeenSnapshot (NoSeenSnapshot, SeenSnapshot, snapshot),
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  Runtime,
  currentState,
  replayEvent,
  start,
 )
import Hydra.HeadLogic.State.Fanout (
  closedToPartialFanout,
  fanoutProgressToClosed,
 )
import Hydra.HeadLogic.State.Types (
  DepositOutput (..),
  HeadOutput (..),
 )
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (HeadId, HeadParameters, HeadSeed)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))

$( singletons
    [d|
      data Phase = Idle | Open | Closed | PartialFanout | Finalized

      lifecycleTopology :: Topology Phase
      lifecycleTopology =
        Topology
          [ (Idle, [Open])
          , (Open, [Open, Closed])
          , (Closed, [Closed, PartialFanout, Finalized])
          , (PartialFanout, [PartialFanout, Finalized])
          , (Finalized, [])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase

deriving via AllVertices Phase instance RenderableVertices Phase

-- | Name protocol operations, including self-loops that change data without
-- changing the Head phase.
transitionLabels :: [((Phase, Phase), String)]
transitionLabels =
  [ ((Idle, Open), "Init")
  , ((Open, Open), "Increment / Decrement")
  , ((Open, Closed), "Close")
  , ((Closed, Closed), "Contest")
  , ((Closed, PartialFanout), "PartialFanout")
  , ((Closed, Finalized), "Fanout")
  , ((PartialFanout, PartialFanout), "PartialFanout")
  , ((PartialFanout, Finalized), "FinalPartialFanout")
  ]

-- | Each constructor owns its phase's payload. Open owns the continuation of
-- the snapshot/transaction composition, not a second mutable copy of its fields.
data State tx phase where
  IdleHead :: Head.IdleState tx -> State tx 'Idle
  OpenHead :: OpenContext tx -> State tx 'Open
  ClosedHead :: Head.ClosedState tx -> State tx 'Closed
  -- The on-chain Head remains Closed until a distribution is observed.
  PendingFanoutHead :: Head.PartialFanoutState tx -> State tx 'Closed
  PartialFanoutHead :: Head.PartialFanoutState tx -> State tx 'PartialFanout
  FinalizedHead :: Head.FinalizedState tx -> State tx 'Finalized

statePhase :: State tx phase -> Phase
statePhase = \case
  IdleHead{} -> Idle
  OpenHead{} -> Open
  ClosedHead{} -> Closed
  PendingFanoutHead{} -> Closed
  PartialFanoutHead{} -> PartialFanout
  FinalizedHead{} -> Finalized

machine :: forall tx m. (IsChainState tx, Applicative m) => Head.HeadState tx -> BaseMachineT m LifecycleTopology (DepositOutput tx) (HeadOutput tx)
machine initial = BaseMachineT (withStateInspector (describeOnly (show . statePhase)) $ restore initial) action
 where
  action :: forall from. State tx from -> DepositOutput tx -> ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  action state input = case state of
    IdleHead st -> case input.event of
      HeadOpened{headId, headSeed, parameters, chainState} -> opened input headId headSeed parameters chainState
      ChainRolledBack{chainState} -> output input $ IdleHead st{Head.chainState}
      _ -> output input state
    OpenHead runtime -> case input.event of
      HeadClosed{chainState, contestationDeadline} ->
        let os = viewOpen runtime
            chs = os.coordinatedHeadState
         in output input $
              ClosedHead
                Head.ClosedState
                  { parameters = os.parameters
                  , confirmedSnapshot = chs.confirmedSnapshot
                  , contestationDeadline
                  , readyToFanoutSent = False
                  , chainState
                  , headId = os.headId
                  , headSeed = os.headSeed
                  , version = chs.version
                  , unconfirmedSnapshot = case chs.seenSnapshot of
                      Head.SeenSnapshot{snapshot} -> Just snapshot
                      _ -> Nothing
                  }
      _ -> output input $ OpenHead $ stepOpen runtime input
    ClosedHead st -> closed st input
    PendingFanoutHead st -> case input.event of
      HeadFanoutReverted{} -> output input $ ClosedHead $ fanoutProgressToClosed st
      HeadContested{} -> closed (fanoutProgressToClosed st) input
      SnapshotConfirmed{snapshot = Just _} -> closed (fanoutProgressToClosed st) input
      HeadIsReadyToFanout{} -> output input $ PendingFanoutHead st{Head.readyToFanoutSent = True}
      _ -> fanout PendingFanoutHead st input
    PartialFanoutHead st -> fanout PartialFanoutHead st input
    FinalizedHead st -> case input.event of
      ChainRolledBack{chainState} -> output input $ FinalizedHead st{Head.chainState}
      _ -> output input state

  closed :: Head.ClosedState tx -> DepositOutput tx -> ActionResult m LifecycleTopology (State tx) 'Closed (HeadOutput tx)
  closed st input = case input.event of
    ChainRolledBack{chainState} -> output input $ ClosedHead st{Head.chainState}
    HeadContested{chainState, contestationDeadline} -> output input $ ClosedHead st{Head.chainState, Head.contestationDeadline}
    SnapshotConfirmed{snapshot = Just snapshot, signatures} ->
      output input $ ClosedHead st{Head.confirmedSnapshot = ConfirmedSnapshot snapshot signatures, Head.unconfirmedSnapshot = Nothing}
    HeadIsReadyToFanout{} -> output input $ ClosedHead st{Head.readyToFanoutSent = True}
    HeadFannedOut{chainState} -> output input $ FinalizedHead Head.FinalizedState{headId = st.headId, chainState}
    HeadFanoutInitiated{remainingOutputs} -> output input $ PendingFanoutHead $ closedToPartialFanout st st.chainState remainingOutputs mempty Head.AutoDrain Head.DatumClosed
    HeadPartialFanoutSelected{remainingOutputs, selection} ->
      output input $ PendingFanoutHead $ closedToPartialFanout st st.chainState remainingOutputs mempty (recordedSelectionMode Head.DatumClosed remainingOutputs selection) Head.DatumClosed
    HeadPartialFannedOut{distributedOutputs, remainingOutputs, chainState, mode} ->
      output input $ PartialFanoutHead $ closedToPartialFanout st chainState remainingOutputs distributedOutputs mode Head.DatumFanoutProgress
    _ -> output input $ ClosedHead st

  -- Cancellation belongs only to PendingFanoutHead. These shared updates cannot
  -- move observed distribution back to Closed or reopen a Head.
  fanout ::
    (AllowedTransition LifecycleTopology from 'Finalized, AllowedTransition LifecycleTopology from 'PartialFanout) =>
    (Head.PartialFanoutState tx -> State tx from) ->
    Head.PartialFanoutState tx ->
    DepositOutput tx ->
    ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  fanout keep st input = case input.event of
    ChainRolledBack{chainState} -> output input $ keep st{Head.chainState}
    HeadFannedOut{chainState} -> output input $ FinalizedHead Head.FinalizedState{headId = st.headId, chainState}
    HeadFanoutInitiated{remainingOutputs} -> output input $ keep st{Head.mode = Head.AutoDrain, Head.remainingOutputs}
    HeadPartialFanoutSelected{remainingOutputs, selection} ->
      output input $ keep st{Head.mode = recordedSelectionMode st.fanoutDatum remainingOutputs selection}
    HeadPartialFannedOut{distributedOutputs = newlyDistributed, remainingOutputs, chainState, mode} ->
      let distributedOutputs = st.distributedOutputs <> newlyDistributed
          next = st{Head.chainState, Head.remainingOutputs, Head.distributedOutputs, Head.mode, Head.fanoutDatum = Head.DatumFanoutProgress}
       in output input $ PartialFanoutHead next
    _ -> output input $ keep st

  opened :: DepositOutput tx -> HeadId -> HeadSeed -> HeadParameters -> ChainStateType tx -> ActionResult m LifecycleTopology (State tx) 'Idle (HeadOutput tx)
  opened input headId headSeed parameters chainState =
    output input $
      OpenHead $
        startOpen
          Head.OpenState
            { coordinatedHeadState =
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
            , ..
            }

  output :: AllowedTransition LifecycleTopology from to => DepositOutput tx -> State tx to -> ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  output DepositOutput{event, deposits} next = pureResult HeadOutput{event, deposits, headState = view next} next

restore :: IsChainState tx => Head.HeadState tx -> InitialState (State tx)
restore = \case
  Head.Idle st -> InitialState $ IdleHead st
  Head.Open os -> InitialState $ OpenHead $ startOpen os
  Head.Closed st -> InitialState $ ClosedHead st
  Head.FanoutProgress st
    | st.fanoutDatum == Head.DatumClosed -> InitialState $ PendingFanoutHead st
    | otherwise -> InitialState $ PartialFanoutHead st
  Head.Finalized st -> InitialState $ FinalizedHead st

view :: State tx phase -> Head.HeadState tx
view = \case
  IdleHead st -> Head.Idle st
  OpenHead runtime -> Head.Open $ viewOpen runtime
  ClosedHead st -> Head.Closed st
  PendingFanoutHead st -> Head.FanoutProgress st
  PartialFanoutHead st -> Head.FanoutProgress st
  FinalizedHead st -> Head.Finalized st

-- | Open-head context around the composed snapshot, transaction and commit owners.
data OpenContext tx = OpenContext HeadParameters (ChainStateType tx) HeadId HeadSeed (Coordinated.Runtime tx)

startOpen :: IsChainState tx => Head.OpenState tx -> OpenContext tx
startOpen os = OpenContext os.parameters os.chainState os.headId os.headSeed (Coordinated.start os)

viewOpen :: OpenContext tx -> Head.OpenState tx
viewOpen (OpenContext parameters chainState headId headSeed runtime) =
  Head.OpenState{coordinatedHeadState = Coordinated.currentState runtime, ..}

stepOpen :: IsChainState tx => OpenContext tx -> DepositOutput tx -> OpenContext tx
stepOpen (OpenContext parameters chainState headId headSeed runtime) input =
  let next = Coordinated.replayEvent input.settledDeposit input.event runtime
      chainState' = case input.event of
        CommitFinalized{chainState = observed} -> observed
        DecommitFinalized{chainState = observed} -> observed
        ChainRolledBack{chainState = observed} -> observed
        _ -> chainState
   in OpenContext parameters chainState' headId headSeed next
