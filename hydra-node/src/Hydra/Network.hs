{-# LANGUAGE TypeApplications #-}
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
  NetworkComponent,
  NetworkCallback,
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

import Hydra.Prelude

import Cardano.Ledger.Shelley.Orphans ()
import Data.IP (IP)
import Hydra.API.Network (
  Host (..),
  NodeId (..),
  readHost,
  readPort,
  showHost,
 )
import Network.Socket (PortNumber, close)
import Network.TypedProtocol.Pipelined ()

-- * Orphans

deriving instance ToJSON IP
deriving instance FromJSON IP

instance ToJSON PortNumber where
  toJSON = toJSON . toInteger

instance FromJSON PortNumber where
  parseJSON = fmap fromInteger . parseJSON

instance Arbitrary PortNumber where
  arbitrary = fromIntegral @Word16 <$> arbitrary

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
