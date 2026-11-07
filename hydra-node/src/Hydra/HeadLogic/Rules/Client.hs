{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Client where

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (LogicError (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (Effect (..))
import Hydra.HeadLogic.Program (Program, emit, reject)
import Hydra.HeadLogic.Rules.Close (onOpenClientClose)
import Hydra.HeadLogic.Rules.Decommit qualified as Decommit
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Rules.Fanout (
  onClosedClientFanout,
  onClosedClientPartialFanout,
  onPartialFanoutClientPartialFanout,
 )
import Hydra.HeadLogic.Rules.Initialise (onIdleClientInit)
import Hydra.HeadLogic.Rules.Recover qualified as Recover
import Hydra.HeadLogic.Rules.SideLoad (
  onOpenClientSideLoadSnapshot,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (CoordinatedHeadState, finalizedCommit),
  HeadState (..),
  OpenState (OpenState, coordinatedHeadState, headId),
 )
import Hydra.HeadLogic.State.Operations (finalizedDepositTxId)
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (ChainPointTime (..), PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (IsTx, TxIdType)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

handleClientInput ::
  IsChainState tx =>
  RuleEnvironment ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current NodeState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  Program tx 'True signing ()
handleClientInput env ChainPointTime{currentSlot} pendingDeposits st ev = case (st, ev) of
  (Idle _, ClientInput Init) ->
    onIdleClientInit env
  -- Open
  (Open openState, ClientInput Close) ->
    onOpenClientClose openState
  (Open openState, ClientInput SafeClose) ->
    onOpenClientClose openState
  (Open{}, ClientInput (NewTx tx)) ->
    onOpenClientNewTx tx
  (Open openState@OpenState{headId = ourHeadId}, ClientInput (SideLoadSnapshot confirmedSnapshot)) ->
    let Snapshot{headId = otherHeadId} = getSnapshot confirmedSnapshot
     in if ourHeadId == otherHeadId
          then onOpenClientSideLoadSnapshot openState confirmedSnapshot
          else reject NotOurHead{ourHeadId, otherHeadId}
  (Open OpenState{headId, coordinatedHeadState}, ClientInput Decommit{decommitTx}) -> do
    Workflow.run $ Decommit.machine headId currentSlot coordinatedHeadState decommitTx
  -- Closed
  (Closed closedState, ClientInput Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState, ClientInput PartialFanout{utxoToFanout}) ->
    onClosedClientPartialFanout closedState utxoToFanout
  -- PartialFanout: once a partial fanout has started, only further
  -- 'PartialFanout' commands are accepted (a plain 'Fanout' falls through to the
  -- general 'CommandFailed' below).
  (FanoutProgress partialFanoutState, ClientInput PartialFanout{utxoToFanout}) ->
    onPartialFanoutClientPartialFanout partialFanoutState utxoToFanout
  -- Node-level
  (_, ClientInput Recover{recoverTxId}) -> do
    Workflow.run $ Recover.machine currentSlot pendingDeposits (openFinalizedDeposit st) recoverTxId
  -- General
  (_, ClientInput{clientInput}) ->
    emit . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    reject $ UnhandledInput ev st

openFinalizedDeposit :: IsTx tx => HeadState tx -> Maybe (TxIdType tx)
openFinalizedDeposit = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{finalizedCommit}} -> finalizedDepositTxId finalizedCommit
  _ -> Nothing

onOpenClientNewTx :: tx -> Program tx ledger signing ()
onOpenClientNewTx tx = emit $ NetworkEffect $ ReqTx tx
