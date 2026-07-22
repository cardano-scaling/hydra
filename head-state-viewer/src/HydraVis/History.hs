{-# LANGUAGE UndecidableInstances #-}

-- | Read 'StateEvent's from a SQLite event store written by a Hydra node and
-- fold them into a scrub-able history of 'NodeState' snapshots.
--
-- The Hydra node persists events using the schema defined by
-- "Hydra.Events.SQLiteBased": a single @events@ table with
-- @event_id INTEGER PRIMARY KEY@ and a @event_data BLOB@ column that holds a
-- JSON-encoded @StateEvent tx@. We open the database read-only (so a running
-- node can keep writing) and replay the events in event-id order.
module HydraVis.History (
  HistoryStep (..),
  loadHistoryFor,
  buildHistory,
  loadEventsAfter,
  extendHistory,
) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple (Only (..), SQLData (..), open, query, query_)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Events (EventId)
import Hydra.HeadLogic (aggregateNodeState)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Node.State (NodeState)

-- | One entry in the scrub-able history: the persisted event and the
-- 'NodeState' that results from folding it into the previous state.
data HistoryStep tx = HistoryStep
  { event :: StateEvent tx
  , stateAfter :: NodeState tx
  }
  deriving stock (Generic)

deriving stock instance (IsChainState tx, Eq (NodeState tx)) => Eq (HistoryStep tx)
deriving stock instance (IsChainState tx, Show (NodeState tx)) => Show (HistoryStep tx)

-- | Read every event row from the SQLite database at @path@, decode each as
-- a @StateEvent tx@, and fold them through 'aggregateNodeState' starting from
-- @initial@. Returns one 'HistoryStep' per persisted event in event-id order.
--
-- Decoding errors are surfaced as 'IOError' exceptions; we do not try to
-- recover. If the file does not exist the underlying 'open' call throws.
loadHistoryFor ::
  forall tx.
  (FromJSON (StateEvent tx), IsChainState tx) =>
  NodeState tx ->
  FilePath ->
  IO [HistoryStep tx]
loadHistoryFor initial path = do
  conn <- open path
  rows <-
    query_
      conn
      "SELECT event_data FROM events ORDER BY event_id ASC" ::
      IO [Only EventBlob]
  SQL.close conn
  events <- forM rows $ \(Only (EventBlob blob)) ->
    case Aeson.eitherDecodeStrict' blob of
      Right e -> pure e
      Left err ->
        fail $
          "HydraVis.History: failed to decode event in " <> path <> ": " <> err
  pure (buildHistory initial events)

-- | Wrapper that accepts both BLOB (the production format) and TEXT (what
-- a quick @sqlite3 CLI@ insert produces) columns when reading event payloads.
newtype EventBlob = EventBlob ByteString

instance FromField EventBlob where
  fromField f@(Field d _) = case d of
    SQLBlob bs -> Ok (EventBlob bs)
    SQLText t -> Ok (EventBlob (TE.encodeUtf8 t))
    _ -> returnError ConversionFailed f "expected event_data to be BLOB or TEXT"

-- | Fold a list of 'StateEvent's through 'aggregateNodeState' to build the
-- per-step history. Pure, exposed for testing.
buildHistory ::
  forall tx.
  IsChainState tx =>
  NodeState tx ->
  [StateEvent tx] ->
  [HistoryStep tx]
buildHistory initial events =
  let step :: NodeState tx -> StateEvent tx -> NodeState tx
      step s e = aggregateNodeState s (stateChanged e)
      states = drop 1 $ scanl' step initial events
   in zipWith HistoryStep events states

-- | Read events with @event_id > lastSeen@ (or every event when 'Nothing').
-- Used by the follow loop to fetch only newly persisted rows on each poll.
loadEventsAfter ::
  forall tx.
  FromJSON (StateEvent tx) =>
  FilePath ->
  Maybe EventId ->
  IO [StateEvent tx]
loadEventsAfter path lastSeen = do
  conn <- open path
  rows <- case lastSeen of
    Nothing ->
      query_
        conn
        "SELECT event_data FROM events ORDER BY event_id ASC" ::
        IO [Only EventBlob]
    Just eid ->
      query
        conn
        "SELECT event_data FROM events WHERE event_id > ? ORDER BY event_id ASC"
        (Only eid) ::
        IO [Only EventBlob]
  SQL.close conn
  forM rows $ \(Only (EventBlob blob)) ->
    case Aeson.eitherDecodeStrict' blob of
      Right e -> pure e
      Left err ->
        fail $
          "HydraVis.History: failed to decode event in " <> path <> ": " <> err

-- | Extend an existing history with newly observed events, folding them
-- through 'aggregateNodeState' starting from the final state of the existing
-- history (or @fallback@ when the history is empty).
extendHistory ::
  IsChainState tx =>
  NodeState tx ->
  [HistoryStep tx] ->
  [StateEvent tx] ->
  [HistoryStep tx]
extendHistory fallback existing newEvents =
  let priorState = case nonEmpty existing of
        Nothing -> fallback
        Just ne -> stateAfter (last ne)
   in existing <> buildHistory priorState newEvents
