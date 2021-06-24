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
  NetworkCallback,
  IP,
  Host,
  showHost,
  readHost,
  PortNumber,
  readPort,

  -- * Utility functions
  close,
) where

import Hydra.Prelude

import Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import Data.IP (IP)
import Hydra.HeadLogic (HydraMessage (..), Snapshot (..), Host)
import Hydra.Ledger (UTxO)
import Network.Socket (PortNumber, close)
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

showHost :: (IsString s, Semigroup s) => Host -> s
showHost (hostname, port) = fromString hostname <> "@" <> show port

readHost :: MonadFail m => String -> m Host
readHost s =
  case break (== '@') s of
    (h, '@' : p) -> (h,) <$> readPort p
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

deriving stock instance Generic (HydraMessage tx)

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (HydraMessage tx) where
  toCBOR = \case
    ReqTx tx -> toCBOR ("ReqTx" :: Text) <> toCBOR tx
    AckTx party tx -> toCBOR ("AckTx" :: Text) <> toCBOR party <> toCBOR tx
    ReqSn party sn txs -> toCBOR ("ReqSn" :: Text) <> toCBOR party <> toCBOR sn <> toCBOR txs
    AckSn party sig sn -> toCBOR ("AckSn" :: Text) <> toCBOR party <> toCBOR sig <> toCBOR sn
    Ping host -> toCBOR ("Ping" :: Text) <> toCBOR (showHost @Text host)

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{number, utxo, confirmed} = toCBOR number <> toCBOR utxo <> toCBOR confirmed

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (HydraMessage tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "AckTx" -> AckTx <$> fromCBOR <*> fromCBOR
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "Ping" -> Ping <$> (fromCBOR @Text >>= readHost . toString)
      msg -> fail $ show msg <> " is not a proper CBOR-encoded HydraMessage"

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (Snapshot tx) where
  fromCBOR = Snapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR
