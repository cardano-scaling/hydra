{-# LANGUAGE DuplicateRecordFields #-}

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
module Hydra.Node.Network (withNetwork, withFlipHeartbeats) where

import Hydra.Prelude hiding (fromList)

import Control.Tracer (Tracer)
import Hydra.Crypto (HydraKey, SigningKey)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Network (Host (..), IP, NetworkComponent, NodeId, PortNumber)
import Hydra.Network.Authenticate (Authenticated (Authenticated), Signed, withAuthentication)
import Hydra.Network.Heartbeat (ConnectionMessages, Heartbeat (..), withHeartbeat)
import Hydra.Network.Ouroboros (TraceOuroborosNetwork, WithHost, withOuroborosNetwork)
import Hydra.Network.Reliability (ReliableMsg, withReliability)
import Hydra.Party (Party, deriveParty)
import Hydra.Persistence (PersistenceIncremental)

-- | An alias for logging messages output by network component.
-- The type is made complicated because the various subsystems use part of the tracer only.
type LogEntry tx msg = HydraLog tx (WithHost (TraceOuroborosNetwork (Signed (ReliableMsg (Heartbeat msg)))))

withNetwork ::
  (ToCBOR msg, ToJSON msg, FromCBOR msg) =>
  -- | Tracer to use for logging messages.
  Tracer IO (LogEntry tx msg) ->
  -- | Callback/observer for connectivity changes in peers.
  ConnectionMessages IO ->
  -- | Persistence handle to store messages
  PersistenceIncremental msg IO ->
  -- | This node's signing key. This is used to sign messages sent to peers.
  SigningKey HydraKey ->
  -- | The list of peers `Party` known to this node.
  [Party] ->
  -- | IP address to listen on for incoming connections.
  IP ->
  -- | Port to listen on.
  PortNumber ->
  -- | Addresses and ports of remote peers.
  [Host] ->
  -- | This node's id.
  NodeId ->
  -- | Produces a `NetworkComponent` that can send `msg` and consumes `Authenticated` @msg@.
  NetworkComponent IO (Authenticated msg) msg ()
withNetwork tracer persistence connectionMessages signingKey otherParties host port peers nodeId =
  let localhost = Host{hostname = show host, port}
      me = deriveParty signingKey
   in withHeartbeat nodeId connectionMessages $
        withFlipHeartbeats $
          withReliability (contramap Reliability tracer) persistence me otherParties $
            withAuthentication (contramap Authentication tracer) signingKey otherParties $
              withOuroborosNetwork (contramap Network tracer) localhost peers

withFlipHeartbeats ::
  NetworkComponent m (Authenticated (Heartbeat msg)) msg1 a ->
  NetworkComponent m (Heartbeat (Authenticated msg)) msg1 a
withFlipHeartbeats withBaseNetwork callback =
  withBaseNetwork unwrapHeartbeats
 where
  unwrapHeartbeats = \case
    Authenticated (Data nid msg) party -> callback $ Data nid (Authenticated msg party)
    Authenticated (Ping nid) _ -> callback $ Ping nid
