{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Client where

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput (
  ClientMessage (CommandFailed),
 )
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Error (LogicError (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (
  Effect (ClientEffect, NetworkEffect, OnChainEffect, postChainTx),
 )
import Hydra.HeadLogic.Program (Program, emit, reject)
import Hydra.HeadLogic.Rules.Client.Decommit (handleDecommit)
import Hydra.HeadLogic.Rules.Client.Fanout (
  onClosedClientFanout,
  onClosedClientPartialFanout,
  onPartialFanoutClientPartialFanout,
 )
import Hydra.HeadLogic.Rules.Client.Recover (
  handleRecover,
 )
import Hydra.HeadLogic.Rules.Client.SideLoad (
  onOpenClientSideLoadSnapshot,
 )
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (..),
  headParametersFor,
 )
import Hydra.HeadLogic.Snapshot.Settlement (finalizedDepositTxId)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    finalizedCommit,
    version
  ),
  HeadState (..),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
 )
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (ChainPointTime (..), PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (IsTx (TxIdType))
import Hydra.Tx.Snapshot (
  Snapshot (Snapshot, headId),
  getSnapshot,
 )

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
  Program tx ()
handleClientInput env ChainPointTime{currentSlot} pendingDeposits st ev = case (st, ev) of
  (Idle _, ClientInput Init) ->
    onIdleClientInit env
  (Finalized{}, ClientInput Init) ->
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
    handleDecommit env.snapshotLimits headId currentSlot coordinatedHeadState decommitTx
  -- Closed
  (Closed closedState, ClientInput Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState, ClientInput PartialFanout{utxoToFanout}) ->
    onClosedClientPartialFanout closedState utxoToFanout
  (FanoutProgress partialFanoutState, ClientInput PartialFanout{utxoToFanout}) ->
    onPartialFanoutClientPartialFanout partialFanoutState utxoToFanout
  -- Node-level
  (_, ClientInput Recover{recoverTxId}) -> do
    handleRecover currentSlot pendingDeposits (openFinalizedDeposit st) recoverTxId
  -- General
  (_, ClientInput{clientInput}) ->
    emit . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    reject $ UnhandledInput ev st

openFinalizedDeposit :: IsTx tx => HeadState tx -> Maybe (TxIdType tx)
openFinalizedDeposit = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{finalizedCommit}} ->
    finalizedDepositTxId finalizedCommit
  _ -> Nothing

onOpenClientNewTx :: tx -> Program tx ()
onOpenClientNewTx tx = emit $ NetworkEffect $ ReqTx tx

onIdleClientInit ::
  RuleEnvironment ->
  Program tx ()
onIdleClientInit env =
  emit OnChainEffect{postChainTx = InitTx{participants, headParameters}}
 where
  headParameters = headParametersFor env

  RuleEnvironment{participants} = env

onOpenClientClose :: OpenState tx -> Program tx ()
onOpenClientClose OpenState{headId, parameters, coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot, version}} =
  emit $ OnChainEffect CloseTx{headId, headParameters = parameters, openVersion = version, closingSnapshot = confirmedSnapshot}
