module Hydra.Events.ApiSink where

import Hydra.Prelude

import Hydra.API.Server (Server (..))
import Hydra.Events (EventSink (..), StateEvent (..))
import Hydra.Node (HydraNode (..))

wireApiEvents ::
  MonadSTM m =>
  Server tx m ->
  m (EventSink (StateEvent tx) m)
wireApiEvents server = do
  let putEvent stateEvt = do
        sendOutput stateEvt
  pure EventSink{putEvent}
 where
  Server{sendOutput} = server

addApiEventSink :: EventSink (StateEvent tx) m -> HydraNode tx m -> HydraNode tx m
addApiEventSink sink node = node{eventSinks = sink : eventSinks node}
