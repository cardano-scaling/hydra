{-# LANGUAGE DuplicateRecordFields #-}

-- TODO: should we still use Heartbeat for peer-specific connectivity? It was
-- always only indicating incoming connetivity (although now with etcd we can
-- assume it working both ways if we see connectivity)

-- | Concrete `Hydra.Network` stack used in a hydra-node.
--
-- This module provides a `withNetwork` function which is the composition of several layers in order to provide various capabilities:
--
--   * `withAuthentication` handles messages' authentication and signature verification
--   * `withEtcdNetwork` uses an 'etcd' cluster to implement reliable broadcast
--
-- The following diagram details the various types of messages each layer is
-- exchanging with its predecessors and successors.
--
-- @
--
--           ▲
--           │                        │
--       Authenticated msg           msg
--           │                        │
--           │                        │
-- ┌─────────┼────────────────────────▼──────┐
-- │                                         │
-- │               Authenticate              │
-- │                                         │
-- └─────────▲────────────────────────┼──────┘
--           │                        │
--           │                        │
--          msg                      msg
--           │                        │
-- ┌─────────┼────────────────────────▼──────┐
-- │                                         │
-- │                   Etcd                  │
-- │                                         │
-- └─────────▲────────────────────────┼──────┘
--           │                        │
--           │        (bytes)         │
--           │                        ▼
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
