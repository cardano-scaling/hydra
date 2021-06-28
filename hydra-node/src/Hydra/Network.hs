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

  -- * Utility functions
  close,
) where

import Hydra.Prelude hiding (show)

import Data.Aeson (object, withObject, (.:), (.=))
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
type NetworkComponent m msg = NetworkCallback msg m -> (Network m msg -> m ()) -> m ()

-- * Types used by concrete implementations

data Host = Host
  { hostName :: Text
  , portNumber :: PortNumber
  }
  deriving (Eq)

instance Show Host where
  show = showHost

instance Read Host where
  readsPrec _ s = case readHost s of
    Just h -> [(h, "")]
    Nothing -> []

instance ToJSON Host where
  toJSON h =
    object
      [ "hostname" .= hostName h
      , "portNumber" .= fromIntegral @_ @Integer (portNumber h)
      ]

instance FromJSON Host where
  parseJSON = withObject "Host" $ \obj ->
    Host
      <$> (obj .: "hostname")
      <*> (fromIntegral @Integer <$> (obj .: "portNumber"))

instance ToCBOR Host where
  toCBOR Host{hostName, portNumber} = toCBOR hostName <> toCBOR (toInteger portNumber)

instance FromCBOR Host where
  fromCBOR = Host <$> fromCBOR <*> (fromInteger <$> fromCBOR)

showHost :: Host -> String
showHost Host{hostName, portNumber} = unpack hostName <> "@" <> show portNumber

readHost :: String -> Maybe Host
readHost s =
  case break (== '@') s of
    (h, '@' : p) -> Host (pack h) <$> readPort p
    _ -> fail $ "readHost: missing @ in " <> s

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
