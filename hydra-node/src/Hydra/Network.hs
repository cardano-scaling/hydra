{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Interface to the Hydra network and base types. Concrete implementations are
-- provided by submodules. Import those instead of this one if interested in
-- actually configuring and running a real network layer.
module Hydra.Network (
  -- * Types
  Network (..),
  NetworkComponent,
  PortNumber,
  Host,
  NetworkCallback,
  IP,

  -- * Parser
  readHost,
  readPort,
) where

import Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Prelude hiding (STM)
import Control.Monad (fail)
import Data.Functor.Contravariant (Contravariant (..))
import Data.IP (IP)
import qualified Data.List as List
import Data.String (String)
import Hydra.HeadLogic (HydraMessage (..), Snapshot (..))
import Hydra.Ledger (UTxO)
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol.Pipelined ()

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

type Host = (HostName, PortNumber)

readHost :: String -> Maybe Host
readHost s =
  case List.break (== '@') s of
    (h, '@' : p) -> (h,) <$> readPort p
    _ -> Nothing

readPort :: String -> Maybe PortNumber
readPort s =
  readMaybe s >>= \n ->
    if n >= 0 && n < fromIntegral (maxBound :: Word16)
      then Just $ fromInteger n
      else Nothing

deriving stock instance Generic (HydraMessage tx)

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (HydraMessage tx) where
  toCBOR = \case
    ReqTx tx -> toCBOR ("ReqTx" :: Text) <> toCBOR tx
    AckTx party tx -> toCBOR ("AckTx" :: Text) <> toCBOR party <> toCBOR tx
    ConfTx -> toCBOR ("ConfTx" :: Text)
    ReqSn sn txs -> toCBOR ("ReqSn" :: Text) <> toCBOR sn <> toCBOR txs
    AckSn party sn -> toCBOR ("AckSn" :: Text) <> toCBOR party <> toCBOR sn
    ConfSn -> toCBOR ("ConfSn" :: Text)
    Ping party -> toCBOR ("Ping" :: Text) <> toCBOR party

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{number, utxo, confirmed} = toCBOR number <> toCBOR utxo <> toCBOR confirmed

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (HydraMessage tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "AckTx" -> AckTx <$> fromCBOR <*> fromCBOR
      "ConfTx" -> pure ConfTx
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR
      "ConfSn" -> pure ConfSn
      "Ping" -> Ping <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded HydraMessage"

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (Snapshot tx) where
  fromCBOR = Snapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR
