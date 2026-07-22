-- | Generate a sample SQLite event store from the canonical SimpleTx script
-- in "HydraVis.Sample".
--
-- Real Hydra nodes write to @<persistenceDir>/hydra.db@ via
-- "Hydra.Events.SQLiteBased"; this module writes a file with the same shape
-- (the @events (event_id, event_data BLOB)@ schema, JSON-encoded
-- @StateEvent tx@ per row) using purely synthesised events so the
-- visualizer can be exercised without a running node.
module HydraVis.SampleDb (writeSampleDb, sampleStateEvents) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Database.SQLite.Simple (close, execute, execute_, open)
import Hydra.Events (EventId)
import Hydra.HeadLogic (aggregateState, update)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.Outcome (Outcome (..), StateChanged)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node.State (NodeState)
import HydraVis.Sample (
  sampleEnvironment,
  sampleInitialState,
  sampleLedger,
  sampleScript,
  sampleStepTime,
 )

-- | Step the sample script through 'HeadLogic.update', collect every emitted
-- 'StateChanged', and tag each with a monotonically increasing 'EventId' and
-- the same wall-clock time we used when stepping it.
sampleStateEvents :: [StateEvent SimpleTx]
sampleStateEvents =
  let pairs :: [(Int, [StateChanged SimpleTx])]
      pairs = collect 1 sampleInitialState sampleScript
   in [ StateEvent
        { eventId = eid
        , stateChanged = sc
        , time = sampleStepTime stepIx
        }
      | (eid, (stepIx, sc)) <- zip [(0 :: EventId) ..] (flatten pairs)
      ]
 where
  flatten :: [(Int, [a])] -> [(Int, a)]
  flatten xs = [(i, x) | (i, ys) <- xs, x <- ys]

  collect ::
    Int ->
    NodeState SimpleTx ->
    [Input SimpleTx] ->
    [(Int, [StateChanged SimpleTx])]
  collect _ _ [] = []
  collect n s (i : rest) =
    let outcome :: Outcome SimpleTx
        outcome = update sampleEnvironment sampleLedger (sampleStepTime n) s i
        scs = case outcome of
          Continue{stateChanges} -> stateChanges
          Wait{stateChanges} -> stateChanges
          Error{} -> []
        s' = aggregateState s outcome
     in (n, scs) : collect (n + 1) s' rest

-- | Create (or recreate) a SQLite database at @path@ holding
-- 'sampleStateEvents'. Overwrites any existing rows.
writeSampleDb :: FilePath -> IO ()
writeSampleDb path = do
  conn <- open path
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS events \
    \(event_id INTEGER NOT NULL PRIMARY KEY, event_data BLOB NOT NULL)"
  execute_ conn "DELETE FROM events"
  forM_ sampleStateEvents $ \e ->
    execute
      conn
      "INSERT INTO events (event_id, event_data) VALUES (?, ?)"
      (eventId e, toStrict (Aeson.encode e))
  close conn
