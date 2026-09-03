-- | The SQLite-backed event store, the recommended persistence backend. The
-- machinery lives in the standalone @event-sourcing@ package; this module
-- re-exports it under the name used throughout hydra-node.
module Hydra.Events.SQLiteBased (module Data.EventSource.SQLite) where

import Data.EventSource.SQLite
