{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Asynchronous messaging interface to the Hydra Network, e.g to other Hydra nodes.
--
-- Concrete implementations are provided by submodules. Import those instead of
-- this one if interested in actually configuring and running a real network
-- layer.
--
-- Incoming and outgoing messages are modelled as 'Message' data type.
module Hydra.Network (
  -- * Types
  Network (..),
  NetworkCallback (..),
  NetworkComponent,
  NetworkConfiguration (..),
  IP,
  Host (..),
  NodeId (..),
  showHost,
  readHost,
  PortNumber,
  readPort,
  Connectivity (..),
  HydraVersionedProtocolNumber (..),
  KnownHydraVersions (..),
  HydraHandshakeRefused (..),

  -- * Utility functions
  close,
) where

import Hydra.Prelude hiding (show)

import Cardano.Ledger.Orphans ()
import Data.IP (IP, toIPv4w)
import Data.Text (pack, unpack)
import Hydra.Cardano.Api (Key (SigningKey))
import Hydra.Tx (Party)
import Hydra.Tx.Crypto (HydraKey)
import Network.Socket (PortNumber, close)
import Test.QuickCheck (elements, listOf, suchThat)
import Test.QuickCheck.Instances.Natural ()
import Text.Read (Read (readsPrec))
import Text.Show (Show (show))

-- * Hydra network interface

-- | Interface from the application to the network layer.
newtype Network m msg = Network
  { broadcast :: msg -> m ()
  -- ^ Send a `msg` to the whole configured hydra network including ourselves.
  }

-- | Interface from network layer to the application.
-- XXX: Reliably delivering a message in the crash-recovery fault model is
-- tricky. According to "Introduction to Reliable and Secure Distributed
-- Programming" section "2.2.4 Crashes with recoveries" explains that storing to
-- stable storage and just pointing to stored events is a better way.
data NetworkCallback msg m = NetworkCallback
  { deliver :: msg -> m ()
  -- ^ The given `msg` was received from the network.
  , onConnectivity :: Connectivity -> m ()
  -- ^ The given `Connectivity` event was observed by network.
  }

-- | A type tying both inbound and outbound messages sending in a single /Component/.
--
-- A `NetworkComponent` can have different inbound and outbound message types.
type NetworkComponent m inbound outbound a = NetworkCallback inbound m -> (Network m outbound -> m a) -> m a

-- * Types used by concrete implementations

-- | Configuration for a `Node` network layer.
data NetworkConfiguration = NetworkConfiguration
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

-- ** IP (Orphans)

deriving anyclass instance ToJSON IP
deriving anyclass instance FromJSON IP

-- ** PortNumber (Orphans)

readPort :: MonadFail m => String -> m PortNumber
readPort s =
  case readMaybe s of
    Nothing -> fail "cannot read port"
    Just n
      | n >= minPort && n <= maxPort -> pure $ fromInteger n
      | otherwise ->
          fail $
            "readPort: "
              <> show n
              <> " not within valid port range: ("
              <> show minPort
              <> ", "
              <> show maxPort
              <> ")"
 where
  maxPort = fromIntegral (maxBound :: Word16)
  minPort = fromIntegral (minBound :: Word16)

instance ToJSON PortNumber where
  toJSON = toJSON . toInteger

instance FromJSON PortNumber where
  parseJSON = fmap fromInteger . parseJSON

instance Arbitrary PortNumber where
  arbitrary = fromIntegral @Word16 <$> arbitrary

-- ** NodeId

newtype NodeId = NodeId {nodeId :: Text}
  deriving newtype (Eq, Show, IsString, ToString, Read, Ord, ToJSON, FromJSON)

instance Arbitrary NodeId where
  arbitrary =
    NodeId . pack <$> suchThat (listOf (elements ['a' .. 'z'])) (not . null)

instance FromCBOR NodeId where
  fromCBOR = NodeId <$> fromCBOR

instance ToCBOR NodeId where
  toCBOR NodeId{nodeId} = toCBOR nodeId

-- ** Host

-- REVIEW(SN): This is also used in hydra-tui
data Host = Host
  { hostname :: Text
  , port :: PortNumber
  }
  deriving stock (Ord, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR Host where
  toCBOR Host{hostname, port} =
    toCBOR hostname <> toCBOR (toInteger port)

instance FromCBOR Host where
  fromCBOR = do
    hostname <- fromCBOR
    port <- fromInteger <$> fromCBOR
    pure Host{hostname, port}

instance Show Host where
  show = showHost

instance Read Host where
  readsPrec _ s = case readHost s of
    Just h -> [(h, "")]
    Nothing -> []

instance Arbitrary Host where
  arbitrary = do
    ip <- toIPv4w <$> arbitrary
    Host (toText $ show ip) <$> arbitrary

showHost :: Host -> String
showHost Host{hostname, port} =
  unpack hostname <> ":" <> show port

readHost :: MonadFail m => String -> m Host
readHost s =
  case break (== ':') s of
    (h, ':' : p) -> Host (pack h) <$> readPort p
    _ -> fail $ "readHost: missing : in " <> s

-- ** Connectivity & versions

-- TODO: improve these types

data Connectivity
  = -- | Individual peer appeared alive on network.
    PeerConnected {peer :: Host}
  | -- | Individual peer disappeared from network (has not been seen active in a while).
    PeerDisconnected {peer :: Host}
  | -- | Connected to Hydra network.
    NetworkConnected
  | -- | Disconnected from Hydra network.
    NetworkDisconnected
  | HandshakeFailure
      { remoteHost :: Host
      , ourVersion :: HydraVersionedProtocolNumber
      , theirVersions :: KnownHydraVersions
      }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

instance Arbitrary Connectivity where
  arbitrary = genericArbitrary

newtype HydraVersionedProtocolNumber = MkHydraVersionedProtocolNumber {hydraVersionedProtocolNumber :: Natural}
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON)

instance Arbitrary HydraVersionedProtocolNumber where
  arbitrary = genericArbitrary

data KnownHydraVersions
  = KnownHydraVersions {fromKnownHydraVersions :: [HydraVersionedProtocolNumber]}
  | NoKnownHydraVersions
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary KnownHydraVersions where
  arbitrary = genericArbitrary

data HydraHandshakeRefused = HydraHandshakeRefused
  { remoteHost :: Host
  , ourVersion :: HydraVersionedProtocolNumber
  , theirVersions :: KnownHydraVersions
  }
  deriving stock (Eq, Show, Generic)
