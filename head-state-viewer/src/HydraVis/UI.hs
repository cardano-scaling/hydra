-- | The Miso application driving the visualizer UI.
--
-- This module wires the pieces together: it assembles the page from the panel
-- modules under "HydraVis.UI" and builds the Miso 'App'. The domain logic
-- lives in the submodules:
--
-- * "HydraVis.UI.Model" — 'Model', 'Action' and pure state accessors.
-- * "HydraVis.UI.Update" — 'updateModel' / 'applyAction' and subscriptions.
-- * "HydraVis.UI.Widgets" — shared view helpers.
-- * "HydraVis.UI.Flow" — the state-transition diagram.
-- * "HydraVis.UI.Timeline" — the transport toolbar and event markers.
-- * "HydraVis.UI.Panels" — peers, snapshot, sync, messages, event/state, authoring.
module HydraVis.UI (
  Model (..),
  Action (..),
  AuthoringCtx (..),
  mkApp,
  mkModel,
  withTraceLog,
  viewModel,
  applyAction,
  applyInputPure,
) where

import Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.StateEvent (StateEvent)
import Hydra.Node.State (NodeState)
import HydraVis.UI.Flow (viewFlow)
import HydraVis.UI.Model (
  Action (..),
  AuthoringCtx (..),
  Model (..),
  lastSeenIn,
  mkModel,
  withTraceLog,
 )
import HydraVis.UI.Panels (
  viewAuthoring,
  viewEvent,
  viewFanout,
  viewMessages,
  viewPeers,
  viewSnapshot,
  viewState,
  viewStuckBanner,
  viewSync,
  viewTrace,
  viewWaitingBanner,
 )
import HydraVis.UI.Timeline (viewToolbar)
import HydraVis.UI.Update (applyAction, applyInputPure, followSub, playSub, updateModel)
import Miso (App (..), View, defaultEvents)
import Miso.Html (div_, h1_, text)
import Miso.Html.Property (styleInline_)
import Miso.Subscription.Keyboard (arrowsSub)
import Miso.Types (LogLevel (..))

mkApp ::
  ( IsChainState tx
  , FromJSON (StateEvent tx)
  , FromJSON (Input tx)
  , ToJSON (Input tx)
  , ToJSON (NodeState tx)
  , ToJSON (StateChanged tx)
  ) =>
  Model tx ->
  App (Model tx) (Action tx)
mkApp initialModel =
  App
    { initialAction = NoOp
    , model = initialModel
    , update = updateModel
    , view = viewModel
    , subs =
        playSub
          : arrowsSub ArrowSeek
          : case (follow initialModel, dbPath initialModel) of
            (True, Just path) -> [followSub path (lastSeenIn initialModel)]
            _ -> []
    , events = defaultEvents
    , mountPoint = Nothing
    , logLevel = Off
    }

viewModel ::
  (IsChainState tx, ToJSON (Input tx), ToJSON (NodeState tx), ToJSON (StateChanged tx)) =>
  Model tx ->
  View (Action tx)
viewModel m =
  div_
    [styleInline_ "font-family: system-ui, sans-serif; max-width: 900px; margin: 1rem auto; padding: 0 1rem;"]
    [ h1_ [] [text "head-state-viewer"]
    , viewToolbar m
    , viewStuckBanner m
    , viewWaitingBanner m
    , viewFlow m
    , viewPeers m
    , viewSync m
    , viewMessages m
    , viewTrace m
    , viewSnapshot m
    , viewFanout m
    , viewAuthoring m
    , viewEvent m
    , viewState m
    ]
