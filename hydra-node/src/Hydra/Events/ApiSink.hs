module Hydra.Events.ApiSink where

import Hydra.Prelude

import Hydra.API.Server (Server (..))
import Hydra.Events (EventSink (..), StateEvent (..))
import Hydra.Node (HydraNode (..))

addEventSink :: EventSink (StateEvent tx) m -> HydraNode tx m -> HydraNode tx m
addEventSink sink node = node{eventSinks = sink : eventSinks node}
