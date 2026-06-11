-- | Wire the Miso 'App' up to a local Warp server via jsaddle. Run this and
-- point a browser at @http://localhost:PORT@.
module HydraVis.Server (runServer, runMultiServer, runCompareServer) where

import Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.StateEvent (StateEvent)
import Hydra.Node.State (NodeState)
import HydraVis.Compare (mkCompareApp, mkCompareModel)
import HydraVis.History (HistoryStep)
import HydraVis.Multi (MultiModel, mkMultiApp)
import HydraVis.Trace (TraceEntry)
import HydraVis.UI (AuthoringCtx, mkApp, mkModel, withTraceLog)
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
  [TraceEntry] ->
  IO ()
runServer port authoring path isFollowing initialState startCursor steps traceEntries = do
  putTextLn $
    "hydra-vis UI listening on http://localhost:"
      <> show port
      <> " ("
      <> show (length steps)
      <> " events loaded, starting at cursor "
      <> show startCursor
      <> (if null traceEntries then "" else ", " <> show (length traceEntries) <> " trace entries")
      <> (if isFollowing then ", following " <> maybe "?" toText path else "")
      <> ")"
  JSaddle.run
    port
    (startApp (mkApp (withTraceLog traceEntries (mkModel authoring path isFollowing initialState startCursor steps))))

-- | Like 'runServer' but for multi-party visualizations.
runMultiServer ::
  ( IsChainState tx
  , FromJSON (Input tx)
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

-- | Serve the multi-node comparison view from per-node event logs.
runCompareServer ::
  (IsChainState tx, ToJSON (StateChanged tx)) =>
  Int ->
  [(Text, [HistoryStep tx])] ->
  IO ()
runCompareServer port nodes = do
  putTextLn $
    "hydra-vis compare UI listening on http://localhost:"
      <> show port
      <> " ("
      <> show (length nodes)
      <> " nodes)"
  JSaddle.run port (startApp (mkCompareApp (mkCompareModel nodes)))
