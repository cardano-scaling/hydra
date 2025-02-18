{-# LANGUAGE DuplicateRecordFields #-}

-- FIXME: Drop unused components and re-create similar documentation as this
-- TODO: Drop Network.Ouroboros, Network.Reliability and Node.Network

-- | Concrete `Hydra.Network` stack dedicated to running a hydra-node.
--
-- This module provides a `withNetwork` function which is the composition of several layers in order to provide various capabilities:
--
--   * `withHeartbeat` maintains knowledge about peers' connectivity,
--   * `withReliability` deals with connections reliability, handling the case
--     of messages being dropped (but not node crash in general),
--   * `withAuthentication` handles messages' authentication and signature verification,
--   * `withOuroborosNetwork` deals with maintaining individual connections to peers and the nitty-gritty details of messages sending and retrieval.
--
-- The following diagram details the various types of messages each layer is
-- exchanging with its predecessors and successors.
--
-- @
--
--          ▲                                    │
--          │ Authenticate msg                   │ msg
--          │                                    │
-- ┌────────┴────────────────────────────────────▼──────┐
-- │                                                    │
-- │                   Heartbeat                        │
-- │        ▲                                           │
-- └────────┬────────────────────────────────────┼──────┘
--          │                                    │
--          │ Heartbeat (Authenticate msg)       │ Heartbeat msg
--          │                                    │
-- ┌────────┴───────────────┐                    │
-- │                        │                    │
-- │    FlipHeartbeats      │                    │
-- │                        │                    │
-- └────────▲───────────────┘                    │
--          │                                    │
--          │ Authenticate (Heartbeat msg)       │
--          │                                    │
-- ┌────────┴────────────────────────────────────▼──────┐
-- │                                                    │
-- │                   Reliability                      │
-- │                                                    │
-- └─────────▲───────────────────────────────────┼──────┘
--           │                                   │
--      Authenticated (ReliableMsg (Heartbeat msg))    ReliableMsg (Heartbeat msg)
--           │                                   │
-- ┌─────────┼───────────────────────────────────▼──────┐
-- │                                                    │
-- │                  Authenticate                      │
-- │                                                    │
-- └─────────▲───────────────────────────────────┼──────┘
--           │                                   │
--           │                                   │
--       Signed (ReliableMsg (Heartbeat msg))       Signed (ReliableMsg (Heartbeat msg))
--           │                                   │
-- ┌─────────┼───────────────────────────────────▼──────┐
-- │                                                    │
-- │                  Ouroboros                         │
-- │                                                    │
-- └─────────▲───────────────────────────────────┼──────┘
--           │                                   │
--           │           (bytes)                 │
--           │                                   ▼
--
-- @
module Hydra.Node.Network (
  NetworkConfiguration (..),
  withNetwork,
) where

import Hydra.Prelude hiding (fromList, replicate)

import Control.Tracer (Tracer, showTracing, stdoutTracer)
import Hydra.Network (HydraVersionedProtocolNumber (..), NetworkComponent, NetworkConfiguration (..))
import Hydra.Network.Authenticate (AuthLog, Authenticated, withAuthentication)
import Hydra.Network.Etcd (withEtcdNetwork)
import Hydra.Network.Message (Message)
import Hydra.Tx (IsTx)

currentNetworkProtocolVersion :: HydraVersionedProtocolNumber
currentNetworkProtocolVersion = MkHydraVersionedProtocolNumber 1

-- | Starts the network layer of a node, passing configured `Network` to its continuation.
withNetwork ::
  forall tx.
  IsTx tx =>
  -- | Tracer to use for logging messages.
  Tracer IO AuthLog ->
  -- | The network configuration
  NetworkConfiguration ->
  -- | Produces a `NetworkComponent` that can send `msg` and consumes `Authenticated` @msg@.
  -- XXX: This is odd as we map connectivity events into the main 'deliver' data type.
  NetworkComponent IO (Authenticated (Message tx)) (Message tx) ()
withNetwork tracer conf callback action = do
  withAuthentication
    tracer
    signingKey
    otherParties
    -- FIXME: trace authentication and etcd stuff together
    (withEtcdNetwork (showTracing stdoutTracer) currentNetworkProtocolVersion conf)
    callback
    action
 where
  NetworkConfiguration{signingKey, otherParties} = conf
