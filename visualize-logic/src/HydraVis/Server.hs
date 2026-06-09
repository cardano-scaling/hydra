-- | Wire the Miso 'App' up to a local Warp server via jsaddle. Run this and
-- point a browser at @http://localhost:PORT@.
module HydraVis.Server (runServer, runMultiServer) where

import Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.StateEvent (StateEvent)
import Hydra.Node.State (NodeState)
import HydraVis.History (HistoryStep)
import HydraVis.Multi (MultiModel, mkMultiApp)
import HydraVis.UI (AuthoringCtx, mkApp, mkModel)
import Language.Javascript.JSaddle.Warp qualified as JSaddle
import Miso (startApp)

runServer ::
  ( IsChainState tx
  , FromJSON (StateEvent tx)
  , FromJSON (Input tx)
  , ToJSON (Input tx)
  , ToJSON (NodeState tx)
  , ToJSON (StateChanged tx)
  ) =>
  Int ->
  Maybe (AuthoringCtx tx) ->
  Maybe FilePath ->
  Bool ->
  NodeState tx ->
  Int ->
  [HistoryStep tx] ->
  IO ()
runServer port authoring path isFollowing initialState startCursor steps = do
  putTextLn $
    "hydra-vis UI listening on http://localhost:"
      <> show port
      <> " ("
      <> show (length steps)
      <> " events loaded, starting at cursor "
      <> show startCursor
      <> (if isFollowing then ", following " <> maybe "?" toText path else "")
      <> ")"
  JSaddle.run
    port
    (startApp (mkApp (mkModel authoring path isFollowing initialState startCursor steps)))

-- | Like 'runServer' but for multi-party visualizations.
runMultiServer ::
  ( IsChainState tx
  , FromJSON (Input tx)
  , FromJSON (StateEvent tx)
  , ToJSON (Input tx)
  , ToJSON (NodeState tx)
  , ToJSON (StateChanged tx)
  ) =>
  Int ->
  MultiModel tx ->
  IO ()
runMultiServer port mm = do
  putTextLn $
    "hydra-vis multi-party UI listening on http://localhost:" <> show port
  JSaddle.run port (startApp (mkMultiApp mm))
