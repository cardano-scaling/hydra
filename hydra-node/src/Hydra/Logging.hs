-- | Adapter module to the actual logging framework.
-- All Hydra node components implement /Structured logging/ via the
-- [contra-tracer](https://hackage.haskell.org/package/contra-tracer) generic
-- logging framework. All logs are output in JSON.
--
-- The machinery lives in the standalone @contra-tracer-json@ package; this
-- module re-exports it under the name used throughout hydra-node.
module Hydra.Logging (module Control.Tracer.JSON) where

import Control.Tracer.JSON
