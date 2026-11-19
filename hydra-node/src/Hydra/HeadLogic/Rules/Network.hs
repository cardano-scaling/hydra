{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Hydra.HeadLogic.Rules.Network.AckSn (handleAckSn)
import Hydra.HeadLogic.Rules.Network.ReqDec (handleReqDec)
import Hydra.HeadLogic.Rules.Network.ReqSn (handleReqSn)
import Hydra.HeadLogic.Rules.Network.ReqTx (handleReqTx)
import Hydra.HeadLogic.Snapshot.Request (scopedDeposits)
import Hydra.HeadLogic.State (
  HeadState (Open),
  OpenState (OpenState, headId),
 )
import Hydra.Network qualified as Network (Connectivity (..))
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.State (
  ChainPointTime,
  PendingDeposits,
  depositsForHead,
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
  -- Open
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    handleReqTx env chainPointTime openState ttl (scopedDeposits openState pendingDeposits) tx
  -- NOTE: 'ReqSn' gets the unfiltered (per-head) deposits: it distinguishes a
  -- requested deposit that is blocked by a finalized increment (hard error)
  -- from one that is simply not known locally (wait).
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput ttl (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx depositTxId})) ->
    handleReqSn env (depositsForHead ourHeadId pendingDeposits) chainPointTime openState ttl sender sv sn txIds decommitTx depositTxId
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    handleAckSn env chainPointTime (scopedDeposits openState pendingDeposits) openState sender snapshotSignature sn
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    handleReqDec env ttl chainPointTime (scopedDeposits openState pendingDeposits) openState transaction
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
