-- | The event-source\/event-sink abstraction used by the node to load and
-- emit events. The machinery lives in the standalone @event-sourcing@ package;
-- this module re-exports it under the name used throughout hydra-node.
--
-- See 'Hydra.Events.SQLiteBased' for the default implementation.
module Hydra.Events (module Data.EventSource) where

import Data.EventSource
