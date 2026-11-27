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
  module Hydra.Network,
  module Hydra.Network.Types,
  IP,
  PortNumber,
) where

import Hydra.Prelude hiding (show)

import Data.Aeson (withText)
import Data.IP (IP, IPv4, IPv6)
import Data.Secret (Secret)
import Data.Text (unpack)
import Hydra.CBOR.Orphans ()
import Hydra.Cardano.Api (Key (SigningKey))
import Hydra.Network.Types
import Hydra.Tx (Party)
import Hydra.Tx.Crypto (HydraKey)
import Network.Socket (PortNumber)
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

data WhichEtcd = EmbeddedEtcd | SystemEtcd
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR WhichEtcd where
  toCBOR = genericToCBOR

instance FromCBOR WhichEtcd where
  fromCBOR = genericFromCBOR

-- | Configuration for a `Node` network layer.
data NetworkConfiguration = NetworkConfiguration
  { persistenceDir :: FilePath
  -- ^ Persistence directory
  , signingKey :: Secret (SigningKey HydraKey)
  -- ^ This node's signing key. This is used to sign messages sent to peers.
  , otherParties :: [Party]
  -- ^ The list of peers `Party` known to this node.
  , listen :: Host
  -- ^ Address to listen on for incoming connections.
  , advertise :: Host
  -- ^ Address to advertise to peers.
  , peers :: [Host]
  -- ^ Addresses and ports of remote peers.
  , nodeId :: NodeId
  -- ^ This node's id.
  , whichEtcd :: WhichEtcd
  -- ^ Whether to use the system etcd (on the path) or the embedded one.
  }

-- ** IP (Orphans)

-- | Preserve the tagged IP encoding with textual IPv4 and IPv6 fields.
deriving anyclass instance ToJSON IP

deriving anyclass instance FromJSON IP

instance ToJSON IPv4 where
  toJSON = toJSON . show

instance FromJSON IPv4 where
  parseJSON = withText "IPv4" $ \txt ->
    maybe (fail $ "failed to read as IPv4 " <> show txt) pure $ readMaybe (unpack txt)

instance ToJSON IPv6 where
  toJSON = toJSON . show

instance FromJSON IPv6 where
  parseJSON = withText "IPv6" $ \txt ->
    maybe (fail $ "failed to read as IPv6 " <> show txt) pure $ readMaybe (unpack txt)
