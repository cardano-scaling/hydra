-- | Log rotation for an event store. The machinery lives in the standalone
-- @event-sourcing@ package; this module re-exports it under the name used
-- throughout hydra-node.
module Hydra.Events.Rotation (module Data.EventSource.Rotation) where

import Data.EventSource.Rotation
