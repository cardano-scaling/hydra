{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | All input admission goes through this workflow, including development tools.
-- Size checks precede any operation that could echo an unsafe client input.
module Hydra.HeadLogic.Rules.Input (Stage (..), InputTopology, Args (..), workflow) where

import Crem.Topology (Topology (Topology))
import Hydra.API.ClientInput (validateClientInputWith)
import Hydra.API.ServerOutput qualified as ServerOutput (
  ClientMessage (RejectedInputBecauseUnsynced),
 )
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (
  LogicError (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (Effect (..), WaitReason (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  decide,
  emit,
  reject,
  suspend,
 )
import Hydra.HeadLogic.Rules.Chain (handleChainInput)
import Hydra.HeadLogic.Rules.Client (handleClientInput)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.Network (handleNetworkInput)
import Hydra.HeadLogic.State (
  ClosedState,
  FinalizedState,
  HeadState (Closed, FanoutProgress, Finalized, Idle, Open),
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
import Hydra.HeadLogic.Workflow qualified as Workflow (
  workflow,
 )
import Hydra.Node.State (
  ChainPointTime (..),
  NodeState (..),
  SyncedStatus (..),
  syncedStatus,
 )
import Hydra.Prelude hiding (State, state)
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))

data Stage = CheckInputSize | CheckSynchronisation | HandleIdle | HandleOpen | HandleClosed | HandleFanout | HandleFinalized | Finished

type InputTopology =
  'Topology
    '[ '( 'CheckInputSize, '[ 'CheckSynchronisation])
     , '( 'CheckSynchronisation, '[ 'HandleIdle, 'HandleOpen, 'HandleClosed, 'HandleFanout, 'HandleFinalized, 'Finished])
     , '( 'HandleIdle, '[ 'Finished])
     , '( 'HandleOpen, '[ 'Finished])
     , '( 'HandleClosed, '[ 'Finished])
     , '( 'HandleFanout, '[ 'Finished])
     , '( 'HandleFinalized, '[ 'Finished])
     ]
deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  CheckSize :: State tx 'CheckInputSize
  CheckSync :: State tx 'CheckSynchronisation
  InIdle :: IdleState tx -> State tx 'HandleIdle
  InOpen :: OpenState tx -> State tx 'HandleOpen
  InClosed :: ClosedState tx -> State tx 'HandleClosed
  InFanout :: PartialFanoutState tx -> State tx 'HandleFanout
  InFinalized :: FinalizedState tx -> State tx 'HandleFinalized
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  CheckSize -> CheckInputSize
  CheckSync -> CheckSynchronisation
  InIdle{} -> HandleIdle
  InOpen{} -> HandleOpen
  InClosed{} -> HandleClosed
  InFanout{} -> HandleFanout
  InFinalized{} -> HandleFinalized
  Done -> Finished

data Args tx = Args
  { env :: ~RuleEnvironment
  , now :: ~UTCTime
  , nodeState :: ~(NodeState tx)
  , input :: ~(Input tx)
  }

workflow :: forall tx. IsChainState tx => Args tx -> Workflow tx InputTopology
workflow Args{..} = Workflow.workflow (show . statePhase) CheckSize step
 where
  step :: State tx from -> Transition tx InputTopology (State tx) from
  step = \case
    CheckSize -> continue (const CheckSync) $ case input of
      ClientInput clientInput -> case validateClientInputWith env.snapshotLimits clientInput of
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
        Finalized st -> continue (const $ InFinalized st) admitted
    InIdle st -> continue (const Done) $ dispatch (Idle st)
    InOpen st -> continue (const Done) $ dispatch (Open st)
    InClosed st -> continue (const Done) $ dispatch (Closed st)
    InFanout st -> continue (const Done) $ dispatch (FanoutProgress st)
    InFinalized st -> continue (const Done) $ dispatch (Finalized st)
    Done -> finish Done

  admitted :: Program tx ()
  admitted = void $ Program.decide "input is admitted while at this sync status" True
  dispatch st = case input of
    ChainInput{} -> handleChainInput env now nodeState.pendingDeposits st input syncStatus
    ClientInput{} -> handleClientInput env chainTime nodeState.pendingDeposits st input
    NetworkInput{} -> handleNetworkInput env chainTime nodeState.pendingDeposits st input
  syncStatus = syncedStatus nodeState
  chainTime = nodeState.chainPointTime
