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
import Hydra.Network (HydraVersionedProtocolNumber (..), NetworkComponent, NetworkConfiguration (..))
import Hydra.Network.Authenticate (AuthLog, Authenticated, withAuthentication)
import Hydra.Network.Etcd (EtcdLog, withEtcdNetwork)
import Hydra.Network.Message (Message)
import Hydra.Tx (IsTx)
import Hydra.Logging (traceWith)
import Hydra.Network (Host (..), IP, Network (..), NetworkCallback (..), NetworkComponent, NodeId, PortNumber)
import Hydra.Network.Authenticate (Authenticated (..), Signed, withAuthentication)
import Hydra.Network.Message (
  Connectivity (..),
  HydraHandshakeRefused (..),
  HydraVersionedProtocolNumber (..),
  Message,
  NetworkEvent (..),
 )
import Hydra.Node (HydraNodeLog (..))
import Hydra.Node.ParameterMismatch (ParamMismatch (..), ParameterMismatch (..))
import Hydra.Persistence (Persistence (..), createPersistence, createPersistenceIncremental)
import Hydra.Tx (IsTx, Party, deriveParty)
import Hydra.Tx.Crypto (HydraKey, SigningKey)
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO)

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
currentNetworkProtocolVersion :: HydraVersionedProtocolNumber
currentNetworkProtocolVersion = MkHydraVersionedProtocolNumber 1

-- * Tracing

data NetworkLog
  = Authenticate AuthLog
  | Etcd EtcdLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
-- | Create `MessagePersistence` handle to be used by `Reliability` network layer.
--
-- This function will `throw` a `ParameterMismatch` exception if:
--
--   * Some state already exists and is loaded,
--   * The number of parties is not the same as the number of acknowledgments saved.
configureMessagePersistence ::
  (MonadThrow m, FromJSON msg, ToJSON msg, MonadUnliftIO m, MonadThread m) =>
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

instance Arbitrary NetworkLog where
  arbitrary = genericArbitrary
  shrink = genericShrink
