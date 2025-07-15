{-# LANGUAGE DuplicateRecordFields #-}

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
  NetworkLog,
) where

import Hydra.Prelude hiding (fromList, replicate)

import Control.Tracer (Tracer)
import Hydra.Network (NetworkComponent, NetworkConfiguration (..), ProtocolVersion (..))
import Hydra.Network.Authenticate (AuthLog, Authenticated, withAuthentication)
import Hydra.Network.Etcd (EtcdLog, withEtcdNetwork)
import Hydra.Network.Message (Message)
import Hydra.Tx (IsTx)

-- | Starts the network layer of a node, passing configured `Network` to its continuation.
withNetwork ::
  forall tx.
  IsTx tx =>
  -- | Tracer to use for logging messages.
  Tracer IO NetworkLog ->
  -- | The network configuration
  NetworkConfiguration ->
  -- | Produces a `NetworkComponent` that can send `msg` and consumes `Authenticated` @msg@.
  NetworkComponent IO (Authenticated (Message tx)) (Message tx) ()
withNetwork tracer conf callback action = do
  withAuthentication
    (contramap Authenticate tracer)
    signingKey
    otherParties
    (withEtcdNetwork (contramap Etcd tracer) currentNetworkProtocolVersion conf)
    callback
    action
 where
  NetworkConfiguration{signingKey, otherParties} = conf

-- | The latest hydra network protocol version. Used to identify
-- incompatibilities ahead of time.
currentNetworkProtocolVersion :: ProtocolVersion
currentNetworkProtocolVersion = ProtocolVersion 1

-- * Tracing

data NetworkLog
  = Authenticate AuthLog
  | Etcd EtcdLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
  deriving anyclass (FromJSON)
