{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | All input admission goes through this workflow, including development tools.
-- Size checks precede any operation that could echo an unsafe client input.
module Hydra.HeadLogic.Rules.Input (Phase (..), InputTopology, inputTopology, sInputTopology, machine) where

import Crem.BaseMachine (BaseMachineT (..), InitialState (..))
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
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
import Hydra.API.ClientInput (validateClientInput)
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (LogicError (..), SideLoadRequirementFailure (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (Effect (..), WaitReason (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Chain (handleChainInput)
import Hydra.HeadLogic.Rules.Client (handleClientInput)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Rules.Network (handleNetworkInput)
import Hydra.HeadLogic.State (
  ClosedState,
  HeadState (Closed, FanoutProgress, Idle, Open),
  IdleState,
  OpenState,
  PartialFanoutState,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
 )
import Hydra.Node.State (ChainPointTime (..), NodeState (..), SyncedStatus (..), syncedStatus)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))

$( singletons
    [d|
      data Phase = CheckInputSize | CheckSynchronisation | HandleIdle | HandleOpen | HandleClosed | HandleFanout | Finished

      inputTopology :: Topology Phase
      inputTopology =
        Topology
          [ (CheckInputSize, [CheckSynchronisation])
          , (CheckSynchronisation, [HandleIdle, HandleOpen, HandleClosed, HandleFanout, Finished])
          , (HandleIdle, [Finished])
          , (HandleOpen, [Finished])
          , (HandleClosed, [Finished])
          , (HandleFanout, [Finished])
          ]
      |]
 )
deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data State tx phase where
  CheckSize :: State tx 'CheckInputSize
  CheckSync :: State tx 'CheckSynchronisation
  InIdle :: IdleState tx -> State tx 'HandleIdle
  InOpen :: OpenState tx -> State tx 'HandleOpen
  InClosed :: ClosedState tx -> State tx 'HandleClosed
  InFanout :: PartialFanoutState tx -> State tx 'HandleFanout
  Done :: State tx 'Finished

machine :: forall tx. IsChainState tx => RuleEnvironment -> UTCTime -> NodeState tx -> Input tx -> Workflow tx 'True 'True InputTopology
machine env now nodeState input = BaseMachineT (InitialState CheckSize) $ \state () -> step state
 where
  step :: State tx from -> Transition tx 'True 'True InputTopology (State tx) from
  step = \case
    CheckSize -> continue (const CheckSync) $ case input of
      ClientInput clientInput -> case validateClientInput clientInput of
        Left AccumulatorTooLarge{utxoCount, maxAllowed} -> do
          void $ Program.decide "client input fits the accumulator" False
          Program.reject $ SideLoadSnapshotFailed SideLoadUTxOSetTooLarge{utxoCount, maxAllowed}
        Right _ -> void $ Program.decide "client input fits the accumulator" True
      _ -> pure ()
    CheckSync -> case (syncStatus, input) of
      (CatchingUp, ClientInput clientInput) -> continue (const Done) $ do
        void $ Program.decide "node is in sync" False
        Program.emit $ ClientEffect $ ServerOutput.RejectedInputBecauseUnsynced clientInput chainTime.drift
      (CatchingUp, NetworkInput{}) -> continue (const Done) $ do
        void $ Program.decide "node is in sync" False
        Program.suspend WaitOnNodeInSync{currentSlot = chainTime.currentSlot}
      _ -> case nodeState.headState of
        Idle st -> continue (const $ InIdle st) admitted
        Open st -> continue (const $ InOpen st) admitted
        Closed st -> continue (const $ InClosed st) admitted
        FanoutProgress st -> continue (const $ InFanout st) admitted
    InIdle st -> continue (const Done) $ dispatch (Idle st)
    InOpen st -> continue (const Done) $ dispatch (Open st)
    InClosed st -> continue (const Done) $ dispatch (Closed st)
    InFanout st -> continue (const Done) $ dispatch (FanoutProgress st)
    Done -> finish Done

  admitted :: Program.SnapshotProgram tx ()
  admitted = void $ Program.decide "input is admitted while at this sync status" True
  dispatch st = case input of
    ChainInput{} -> handleChainInput env now nodeState.pendingDeposits st input syncStatus
    ClientInput{} -> handleClientInput env chainTime nodeState.pendingDeposits st input
    NetworkInput{} -> handleNetworkInput env chainTime nodeState.pendingDeposits st input
  syncStatus = syncedStatus nodeState
  chainTime = nodeState.chainPointTime
