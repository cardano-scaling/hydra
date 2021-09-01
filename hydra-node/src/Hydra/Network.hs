{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Interface to the Hydra network and base types. Concrete implementations are
-- provided by submodules. Import those instead of this one if interested in
-- actually configuring and running a real network layer.
module Hydra.Network (
  -- * Types
  Network (..),
  NetworkComponent,
  NetworkCallback,
  IP,
  Host (..),
  showHost,
  readHost,
  PortNumber,
  readPort,
  MockChain (..),
  defaultMockChain,

  -- * Utility functions
  close,
) where

import Hydra.Prelude hiding (show)

import Data.IP (IP, toIPv4w)
import Data.Text (pack, unpack)
import Network.Socket (PortNumber, close)
import Network.TypedProtocol.Pipelined ()
import Text.Read (Read (readsPrec))
import Text.Show (Show (show))

-- * Hydra network interface

-- | Handle to interface with the hydra network and send messages "off chain".
newtype Network m msg = Network
  { -- | Send a `msg` to the whole hydra network.
    broadcast :: msg -> m ()
  }

instance Contravariant (Network m) where
  contramap f (Network bcast) = Network $ \msg -> bcast (f msg)

-- | Handle to interface for inbound messages.
type NetworkCallback msg m = msg -> m ()

-- | A type tying both inbound and outbound messages sending in a single /Component/.
type NetworkComponent m msg a = NetworkCallback msg m -> (Network m msg -> m a) -> m a

-- * Types used by concrete implementations

-- ** PortNumber (Orphans)

instance ToJSON PortNumber where
  toJSON = toJSON . toInteger

instance FromJSON PortNumber where
  parseJSON = fmap fromInteger . parseJSON

instance Arbitrary PortNumber where
  arbitrary = fromIntegral @Word16 <$> arbitrary

instance ToCBOR PortNumber where
  toCBOR = toCBOR . toInteger

instance FromCBOR PortNumber where
  fromCBOR = fmap fromInteger fromCBOR

-- ** Host

-- REVIEW(SN): This is also used in hydra-tui
data Host = Host
  { hostname :: Text
  , port :: PortNumber
  }
  deriving (Ord, Generic, Eq, ToJSON, FromJSON)

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

instance ToCBOR Host where
  toCBOR Host{hostname, port} =
    mconcat
      [ toCBOR hostname
      , toCBOR port
      ]

instance FromCBOR Host where
  fromCBOR =
    Host
      <$> fromCBOR
      <*> (fromInteger <$> fromCBOR)

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
    Just n ->
      if n >= 0 && n < maxPort
        then pure $ fromInteger n
        else fail $ "readPort: " <> show n <> " not within " <> show maxPort
 where
  maxPort = fromIntegral (maxBound :: Word16)

-- | Ports definition for Mock Chain client.
-- HACK: This is a temporary solution until we wire in a real chain client.
data MockChain = MockChain
  { mockChainHost :: String
  , syncPort :: PortNumber
  , catchUpPort :: PortNumber
  , postTxPort :: PortNumber
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultMockChain :: MockChain
defaultMockChain =
  MockChain
    { mockChainHost = "localhost"
    , syncPort = 56789
    , catchUpPort = 56790
    , postTxPort = 56791
    }
