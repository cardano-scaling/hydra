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
import Hydra.HeadLogic.Machine.Open qualified as Open
import Hydra.HeadLogic.Machine.Types (DepositOutput (..), HeadOutput (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State qualified as Head
import Hydra.HeadLogic.State.Operations (closedToPartialFanout, fanoutProgressToClosed, nullOutputs, recordedSelectionMode)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (HeadId, HeadParameters, HeadSeed, IsTx (..))
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

-- | Each constructor owns its phase's payload. Open owns the continuation of
-- the snapshot/transaction composition, not a second mutable copy of its fields.
data State tx phase where
  IdleHead :: Head.IdleState tx -> State tx 'Idle
  OpenHead :: Open.OpenHead tx -> State tx 'Open
  ClosedHead :: Head.ClosedState tx -> State tx 'Closed
  ReadyHead :: Head.ClosedState tx -> State tx 'ReadyToFanout
  PendingHead :: Head.PartialFanoutState tx -> State tx 'PendingFanout
  FanoutHead :: Head.PartialFanoutState tx -> State tx 'FanoutProgress

machine :: forall tx m. (IsChainState tx, Applicative m) => Head.HeadState tx -> BaseMachineT m LifecycleTopology (DepositOutput tx) (HeadOutput tx)
machine initial = BaseMachineT (restore initial) action
 where
  action :: forall from. State tx from -> DepositOutput tx -> ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  action state input = case state of
    IdleHead st -> case input.event of
      HeadOpened{headId, headSeed, parameters, chainState} -> opened input headId headSeed parameters chainState
      ChainRolledBack{chainState} -> output input $ IdleHead st{Head.chainState}
      _ -> output input state
    OpenHead runtime -> case input.event of
      HeadOpened{headId, headSeed, parameters, chainState} -> opened input headId headSeed parameters chainState
      HeadClosed{chainState, contestationDeadline} ->
        let os = Open.view runtime
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
      _ -> output input $ OpenHead $ Open.step runtime input
    ClosedHead st -> closed ClosedHead st input
    ReadyHead st -> closed ReadyHead st input
    PendingHead st -> fanout PendingHead st input
    FanoutHead st -> fanout FanoutHead st input

  closed :: (AllowedTransition LifecycleTopology from 'Open, AllowedTransition LifecycleTopology from 'Idle, AllowedTransition LifecycleTopology from 'ReadyToFanout, AllowedTransition LifecycleTopology from 'PendingFanout, AllowedTransition LifecycleTopology from 'FanoutProgress) => (Head.ClosedState tx -> State tx from) -> Head.ClosedState tx -> DepositOutput tx -> ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  closed keep st input = case input.event of
    HeadOpened{headId, headSeed, parameters, chainState} -> opened input headId headSeed parameters chainState
    ChainRolledBack{chainState} -> output input $ keep st{Head.chainState}
    HeadContested{chainState, contestationDeadline} -> output input $ keep st{Head.chainState, Head.contestationDeadline}
    SnapshotConfirmed{snapshot = Just snapshot, signatures} ->
      output input $ keep st{Head.confirmedSnapshot = ConfirmedSnapshot snapshot signatures, Head.unconfirmedSnapshot = Nothing}
    HeadIsReadyToFanout{} -> output input $ ReadyHead st{Head.readyToFanoutSent = True}
    HeadFannedOut{chainState} -> output input $ IdleHead Head.IdleState{chainState}
    HeadFanoutInitiated{remainingOutputs} -> output input $ PendingHead $ closedToPartialFanout st st.chainState remainingOutputs mempty Head.AutoDrain
    HeadPartialFanoutSelected{remainingOutputs, selection} ->
      output input $ PendingHead $ closedToPartialFanout st st.chainState remainingOutputs mempty $ recordedSelectionMode mempty remainingOutputs selection
    HeadPartialFannedOut{distributedOutputs, remainingOutputs, chainState, mode} ->
      let next = closedToPartialFanout st chainState remainingOutputs distributedOutputs mode
       in if nullOutputs distributedOutputs then output input (PendingHead next) else output input (FanoutHead next)
    _ -> output input $ keep st

  fanout :: (AllowedTransition LifecycleTopology from 'Open, AllowedTransition LifecycleTopology from 'Idle, AllowedTransition LifecycleTopology from 'ReadyToFanout, AllowedTransition LifecycleTopology from 'PendingFanout, AllowedTransition LifecycleTopology from 'FanoutProgress) => (Head.PartialFanoutState tx -> State tx from) -> Head.PartialFanoutState tx -> DepositOutput tx -> ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  fanout keep st input = case input.event of
    HeadOpened{headId, headSeed, parameters, chainState} -> opened input headId headSeed parameters chainState
    ChainRolledBack{chainState} -> output input $ keep st{Head.chainState}
    HeadFannedOut{chainState} -> output input $ IdleHead Head.IdleState{chainState}
    HeadFanoutReverted{} -> output input $ ReadyHead $ fanoutProgressToClosed st
    HeadFanoutInitiated{remainingOutputs} -> output input $ keep st{Head.mode = Head.AutoDrain, Head.remainingOutputs}
    HeadPartialFanoutSelected{remainingOutputs, selection} ->
      output input $ keep st{Head.mode = recordedSelectionMode st.distributedOutputs remainingOutputs selection}
    HeadPartialFannedOut{distributedOutputs = newlyDistributed, remainingOutputs, chainState, mode} ->
      let distributedOutputs = st.distributedOutputs <> newlyDistributed
          next = st{Head.chainState, Head.remainingOutputs, Head.distributedOutputs, Head.mode}
       in if nullOutputs distributedOutputs then output input (PendingHead next) else output input (FanoutHead next)
    _ -> output input $ keep st

  opened :: AllowedTransition LifecycleTopology from 'Open => DepositOutput tx -> HeadId -> HeadSeed -> HeadParameters -> ChainStateType tx -> ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  opened input headId headSeed parameters chainState =
    output input $
      OpenHead $
        Open.start
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

  output :: AllowedTransition LifecycleTopology from to => DepositOutput tx -> State tx to -> ActionResult m LifecycleTopology (State tx) from (HeadOutput tx)
  output DepositOutput{event, deposits} next = pureResult HeadOutput{event, deposits, headState = view next} next

restore :: IsChainState tx => Head.HeadState tx -> InitialState (State tx)
restore = \case
  Head.Idle st -> InitialState $ IdleHead st
  Head.Open os -> InitialState $ OpenHead $ Open.start os
  Head.Closed st
    | st.readyToFanoutSent -> InitialState $ ReadyHead st
    | otherwise -> InitialState $ ClosedHead st
  Head.FanoutProgress st
    | nullOutputs st.distributedOutputs -> InitialState $ PendingHead st
    | otherwise -> InitialState $ FanoutHead st

view :: State tx phase -> Head.HeadState tx
view = \case
  IdleHead st -> Head.Idle st
  OpenHead runtime -> Head.Open $ Open.view runtime
  ClosedHead st -> Head.Closed st
  ReadyHead st -> Head.Closed st
  PendingHead st -> Head.FanoutProgress st
  FanoutHead st -> Head.FanoutProgress st
