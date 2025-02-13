module Hydra.Events.Api where

import Hydra.Prelude

import Conduit (mapMC, (.|))
import Control.Concurrent.Class.MonadSTM (newTVarIO, writeTVar)
import Hydra.API.Server (Server (..), mapStateChangedToServerOutput)
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventSink (..), EventSource (..), StateEvent (..))
import Hydra.Events.FileBased (PersistedStateChange (..))
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.Node (HydraNode (..))
import Hydra.Persistence (PersistenceIncremental (..))
import Hydra.Tx (HeadId (UnsafeHeadId), IsTx, txId)

wireApiEvents ::
  (IsChainState tx, MonadSTM m) =>
  Server tx m ->
  PersistenceIncremental (PersistedStateChange tx) m ->
  m (EventSink (StateEvent tx) m)
wireApiEvents server PersistenceIncremental{append, source} = do
  let putEvent StateEvent{stateChanged} =
        maybe (pure ()) sendOutput $ mapStateChangedToServerOutput stateChanged
  pure EventSink{putEvent}
 where
  Server{sendOutput} = server

addApiEventSink :: EventSink (StateEvent tx) m -> HydraNode tx m -> HydraNode tx m
addApiEventSink sink node = node{eventSinks = sink : eventSinks node}
