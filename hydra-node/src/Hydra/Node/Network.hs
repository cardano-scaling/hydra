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
import Hydra.Tx (IsTx, Party)

-- | Starts the network layer of a node, passing configured `Network` to its continuation.
--
-- The 'STM' action 'readAcceptedParties' yields the current set of accepted
-- other parties; it is read on every inbound message. For nodes that don't
-- yet integrate the dynamic-head-participants flow (issue #1813), pass
-- @pure (otherParties conf)@.
withNetwork ::
  forall tx.
  IsTx tx =>
  -- | Tracer to use for logging messages.
  Tracer IO NetworkLog ->
  -- | The network configuration
  NetworkConfiguration ->
  -- | Current set of accepted other parties (read once per inbound message).
  STM IO [Party] ->
  -- | Currently-joining party (Phase 2 of dynamic-head-participants, issue
  -- #1813). 'Just' for the window between recording the pending 'AddParty'
  -- and observing the on-chain 'UpdateParametersTx'. Pass @pure Nothing@
  -- when no join is in flight.
  STM IO (Maybe Party) ->
  -- | Produces a `NetworkComponent` that can send `msg` and consumes `Authenticated` @msg@.
  NetworkComponent IO (Authenticated (Message tx)) (Message tx) ()
withNetwork tracer conf readAcceptedParties readJoiningParty callback action = do
  withAuthentication
    (contramap Authenticate tracer)
    signingKey
    readAcceptedParties
    readJoiningParty
    (withEtcdNetwork (contramap Etcd tracer) currentNetworkProtocolVersion conf)
    callback
    action
 where
  NetworkConfiguration{signingKey} = conf

-- | The latest hydra network protocol version. Used to identify
-- incompatibilities ahead of time.
--
-- Bumped from 1 -> 2 with the dynamic-head-participants feature
-- (issue #1813): 'ReqSn' gains an optional 'parameterUpdate' field and
-- 'ReqLeave' is a new constructor.
currentNetworkProtocolVersion :: ProtocolVersion
currentNetworkProtocolVersion = ProtocolVersion 2

-- * Tracing

data NetworkLog
  = Authenticate AuthLog
  | Etcd EtcdLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
  deriving anyclass (FromJSON)
