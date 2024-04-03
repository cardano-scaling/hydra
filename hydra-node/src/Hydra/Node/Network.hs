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
module Hydra.Node.Network (
  NetworkConfiguration (..),
  withNetwork,
  withFlipHeartbeats,
  configureMessagePersistence,
  acksFile,
) where

import Hydra.Prelude hiding (fromList, replicate)

import Control.Tracer (Tracer)
import Hydra.Crypto (HydraKey, SigningKey)
import Hydra.Ledger (IsTx)
import Hydra.Logging (traceWith)
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Network (Host (..), IP, NetworkComponent, NodeId, PortNumber)
import Hydra.Network.Authenticate (Authenticated (..), Signed, withAuthentication)
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Message (
  Connectivity (..),
  HydraHandshakeRefused (..),
  HydraVersionedProtocolNumber (..),
  Message,
  NetworkEvent (..),
 )
import Hydra.Network.Ouroboros (HydraNetworkConfig (..), TraceOuroborosNetwork, WithHost, withOuroborosNetwork)
import Hydra.Network.Reliability (MessagePersistence, ReliableMsg, mkMessagePersistence, withReliability)
import Hydra.Node (HydraNodeLog (..))
import Hydra.Node.ParameterMismatch (ParamMismatch (..), ParameterMismatch (..))
import Hydra.Party (Party, deriveParty)
import Hydra.Persistence (Persistence (..), createPersistence, createPersistenceIncremental)
import System.FilePath ((</>))

-- | An alias for logging messages output by network component.
-- The type is made complicated because the various subsystems use part of the tracer only.
type LogEntry tx msg = HydraLog tx (WithHost (TraceOuroborosNetwork (Signed (ReliableMsg (Heartbeat msg)))))

-- | Configuration for a `Node` network layer.
data NetworkConfiguration m = NetworkConfiguration
  { persistenceDir :: FilePath
  -- ^ Persistence directory
  , signingKey :: SigningKey HydraKey
  -- ^ This node's signing key. This is used to sign messages sent to peers.
  , otherParties :: [Party]
  -- ^ The list of peers `Party` known to this node.
  , host :: IP
  -- ^ IP address to listen on for incoming connections.
  , port :: PortNumber
  -- ^ Port to listen on.
  , peers :: [Host]
  -- ^ Addresses and ports of remote peers.
  , nodeId :: NodeId
  -- ^ This node's id.
  }

currentHydraVersionedProtocol :: HydraVersionedProtocolNumber
currentHydraVersionedProtocol = MkHydraVersionedProtocolNumber 1

-- | Starts the network layer of a node, passing configured `Network` to its continuation.
withNetwork ::
  forall tx.
  IsTx tx =>
  -- | Tracer to use for logging messages.
  Tracer IO (LogEntry tx (Message tx)) ->
  -- | The network configuration
  NetworkConfiguration IO ->
  -- | Produces a `NetworkComponent` that can send `msg` and consumes `Authenticated` @msg@.
  NetworkComponent IO (NetworkEvent (Message tx)) (Message tx) ()
withNetwork tracer configuration callback action = do
  let localHost = Host{hostname = show host, port}
      me = deriveParty signingKey
      numberOfParties = length $ me : otherParties
  messagePersistence <- configureMessagePersistence (contramap Node tracer) persistenceDir numberOfParties

  let reliability =
        withFlipHeartbeats $
          withReliability (contramap Reliability tracer) messagePersistence me otherParties $
            withAuthentication (contramap Authentication tracer) signingKey otherParties $
              withOuroborosNetwork
                (contramap Network tracer)
                HydraNetworkConfig
                  { protocolVersion = currentHydraVersionedProtocol
                  , localHost
                  , remoteHosts = peers
                  }
                ( \HydraHandshakeRefused{remoteHost, ourVersion, theirVersions} ->
                    callback . ConnectivityEvent $ HandshakeFailure{remoteHost, ourVersion, theirVersions}
                )

  withHeartbeat nodeId reliability (callback . mapHeartbeat) $ \network ->
    action network
 where
  NetworkConfiguration{persistenceDir, signingKey, otherParties, host, port, peers, nodeId} = configuration

  mapHeartbeat :: Either Connectivity (Authenticated (Message tx)) -> NetworkEvent (Message tx)
  mapHeartbeat = \case
    Left connectivity -> ConnectivityEvent connectivity
    Right (Authenticated{payload, party}) -> ReceivedMessage{sender = party, msg = payload}

-- | Create `MessagePersistence` handle to be used by `Reliability` network layer.
--
-- This function will `throw` a `ParameterMismatch` exception if:
--
--   * Some state already exists and is loaded,
--   * The number of parties is not the same as the number of acknowledgments saved.
configureMessagePersistence ::
  (MonadIO m, MonadThrow m, FromJSON msg, ToJSON msg, MonadSTM m, MonadThread m, MonadThrow (STM m)) =>
  Tracer m (HydraNodeLog tx) ->
  FilePath ->
  Int ->
  m (MessagePersistence m msg)
configureMessagePersistence tracer persistenceDir numberOfParties = do
  msgPersistence <- createPersistenceIncremental $ storedMessagesFile persistenceDir
  ackPersistence@Persistence{load} <- createPersistence $ acksFile persistenceDir
  mAcks <- load
  ackPersistence' <- case fmap (\acks -> length acks == numberOfParties) mAcks of
    Just False -> do
      let paramsMismatch = [SavedNetworkPartiesInconsistent{numberOfParties}]
      traceWith tracer (Misconfiguration paramsMismatch)
      throwIO $ ParameterMismatch paramsMismatch
    _ -> pure ackPersistence
  pure $ mkMessagePersistence numberOfParties msgPersistence ackPersistence'

withFlipHeartbeats ::
  NetworkComponent m (Authenticated (Heartbeat inbound)) outbound a ->
  NetworkComponent m (Heartbeat (Authenticated inbound)) outbound a
withFlipHeartbeats withBaseNetwork callback =
  withBaseNetwork unwrapHeartbeats
 where
  unwrapHeartbeats = \case
    Authenticated (Data nid msg) party -> callback $ Data nid (Authenticated msg party)
    Authenticated (Ping nid) _ -> callback $ Ping nid

-- | Where are the messages stored, relative to given directory.
storedMessagesFile :: FilePath -> FilePath
storedMessagesFile = (</> "network-messages")

-- | Where is the acknowledgments vector stored, relative to given directory.
acksFile :: FilePath -> FilePath
acksFile = (</> "acks")
