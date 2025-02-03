{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Asynchronous messaging interface to the Hydra Network, e.g to other Hydra nodes.
--
-- Concrete implementations are
-- provided by submodules. Import those instead of this one if interested in
-- actually configuring and running a real network layer.
--
-- Incoming and outgoing messages are modelled as 'Message' data type.
module Hydra.Network (
  -- * Types
  Network (..),
  NetworkCallback (..),
  NetworkComponent,
  IP,
  Host (..),
  NodeId (..),
  showHost,
  readHost,
  PortNumber,
  readPort,

  -- * Utility functions
  close,
) where

import Hydra.Prelude hiding (show)

import Cardano.Ledger.Orphans ()
import Data.IP (IP, toIPv4w)
import Data.Text (pack, unpack)
import Network.Socket (PortNumber, close)
import Test.QuickCheck (elements, listOf, suchThat)
import Text.Read (Read (readsPrec))
import Text.Show (Show (show))

deriving anyclass instance ToJSON IP
deriving anyclass instance FromJSON IP

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
newtype NetworkCallback msg m = NetworkCallback
  { deliver :: msg -> m ()
  -- ^ The given `msg` was received from the network.
  }

-- | A type tying both inbound and outbound messages sending in a single /Component/.
--
-- A `NetworkComponent` can have different inbound and outbound message types.
type NetworkComponent m inbound outbound a = NetworkCallback inbound m -> (Network m outbound -> m a) -> m a

-- * Types used by concrete implementations

-- ** PortNumber (Orphans)

instance ToJSON PortNumber where
  toJSON = toJSON . toInteger

instance FromJSON PortNumber where
  parseJSON = fmap fromInteger . parseJSON

instance Arbitrary PortNumber where
  arbitrary = fromIntegral @Word16 <$> arbitrary

newtype NodeId = NodeId {nodeId :: Text}
  deriving newtype (Eq, Show, IsString, Read, Ord, ToJSON, FromJSON)

instance Arbitrary NodeId where
  arbitrary =
    NodeId . pack <$> suchThat (listOf (elements ['a' .. 'z'])) (not . null)

-- return $ NodeId $ pack c

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
