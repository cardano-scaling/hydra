{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Network where

import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (LogicError (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, reject)
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (configuredPeers),
 )
import Hydra.HeadLogic.Rules.Network.AckSn qualified as AckSn
import Hydra.HeadLogic.Rules.Network.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Rules.Network.ReqSn qualified as ReqSn
import Hydra.HeadLogic.Rules.Network.ReqTx qualified as ReqTx
import Hydra.HeadLogic.State (
  HeadState (Open),
  OpenState (OpenState, headId),
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (run)
import Hydra.Network qualified as Network (Connectivity (..))
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.State (
  ChainPointTime,
  PendingDeposits,
  depositsForHead,
  depositsForHeadWithoutFinalizedCommit,
 )
import Hydra.Prelude

handleNetworkInput ::
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
handleNetworkInput env chainPointTime pendingDeposits st ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent env.configuredPeers conn
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx{..}})) ->
    Workflow.run $
      ReqTx.workflow ReqTx.Args{pendingDeposits = depositsForHeadWithoutFinalizedCommit openState pendingDeposits, ..}
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput ttl (ReceivedMessage{sender, msg = ReqSn{..}})) ->
    Workflow.run $
      ReqSn.workflow ReqSn.Args{pendingDeposits = depositsForHead ourHeadId pendingDeposits, ..}
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn{..}})) ->
    Workflow.run $
      AckSn.workflow AckSn.Args{pendingDeposits = depositsForHeadWithoutFinalizedCommit openState pendingDeposits, ..}
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{..}})) ->
    Workflow.run $
      ReqDec.workflow ReqDec.Args{pendingDeposits = depositsForHeadWithoutFinalizedCommit openState pendingDeposits, ..}
  _ ->
    reject $ UnhandledInput ev st

onConnectionEvent :: Text -> Network.Connectivity -> Program tx ()
onConnectionEvent misconfiguredPeers = \case
  Network.NetworkConnected ->
    change NetworkConnected
  Network.NetworkDisconnected ->
    change NetworkDisconnected
  Network.VersionMismatch{ourVersion, theirVersion} ->
    change NetworkVersionMismatch{ourVersion, theirVersion}
  Network.ClusterIDMismatch{clusterPeers} ->
    change NetworkClusterIDMismatch{clusterPeers, misconfiguredPeers}
  Network.PeerConnected{peer} ->
    change PeerConnected{peer}
  Network.PeerDisconnected{peer} ->
    change PeerDisconnected{peer}
  Network.BroadcastStalled{pendingBroadcasts, stallReason} ->
    change NetworkBroadcastStalled{pendingBroadcasts, stallReason}
  Network.BroadcastResumed ->
    change NetworkBroadcastResumed
