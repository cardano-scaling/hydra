module Hydra.Events.ApiSink where

import Hydra.Prelude

import Conduit (MonadUnliftIO, mapM_C, runConduitRes, (.|))
import Hydra.API.Server (Server (..), mapStateChangedToServerOutput)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventSink (..), StateEvent (..))
import Hydra.Events.FileBased (PersistedStateChange (..))
import Hydra.Node (HydraNode (..))
import Hydra.Persistence (PersistenceIncremental (..))

wireApiEvents ::
  (IsChainState tx, MonadSTM m, MonadUnliftIO m) =>
  PersistenceIncremental (PersistedStateChange tx) m ->
  Server tx m ->
  m (EventSink (StateEvent tx) m)
wireApiEvents persistence server = do
  let putEvent stateEvt@StateEvent{stateChanged} =
        case mapStateChangedToServerOutput stateChanged of
          Nothing -> pure ()
          Just output -> do
            append (New stateEvt)
            sendOutput output
  -- source all persisted events to the API server
  -- NOTE: we do the same sourcing in 'hydrate' but there we don't have the access to the server yet.
  runConduitRes $
    source
      .| mapM_C
        ( \case
            New StateEvent{stateChanged} ->
              case mapStateChangedToServerOutput stateChanged of
                Nothing -> pure ()
                Just a -> lift $ sendOutput a
            _ -> pure ()
        )
  pure EventSink{putEvent}
 where
  PersistenceIncremental{append, source} = persistence
  Server{sendOutput} = server

addApiEventSink :: EventSink (StateEvent tx) m -> HydraNode tx m -> HydraNode tx m
addApiEventSink sink node = node{eventSinks = sink : eventSinks node}
