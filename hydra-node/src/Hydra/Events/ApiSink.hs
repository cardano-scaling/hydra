module Hydra.Events.ApiSink where

import Hydra.Prelude

import Hydra.API.Server (Server (..), mapStateChangedToServerOutput)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventSink (..), StateEvent (..))
import Hydra.Node (HydraNode (..))

wireApiEvents ::
  (IsChainState tx, MonadSTM m) =>
  Server tx m ->
  m (EventSink (StateEvent tx) m)
wireApiEvents server = do
  let putEvent StateEvent{stateChanged} =
        maybe (pure ()) sendOutput $ mapStateChangedToServerOutput stateChanged
  pure EventSink{putEvent}
 where
  Server{sendOutput} = server

addApiEventSink :: EventSink (StateEvent tx) m -> HydraNode tx m -> HydraNode tx m
addApiEventSink sink node = node{eventSinks = sink : eventSinks node}
