{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Interface to the Hydra network and base types. Concrete implementations are
-- provided by submodules. Import those instead of this one if interested in
-- actually configuring and running a real network layer.
module Hydra.Network (
  -- * Types
  HydraNetwork (..),
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
import Codec.Serialise
import Control.Monad (fail)
import Data.Functor.Contravariant (Contravariant (..))
import Data.IP (IP)
import qualified Data.List as List
import Data.String (String)
import Hydra.HeadLogic (HydraMessage (..))
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol.Pipelined ()

-- * Hydra network interface

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork m msg = HydraNetwork
  { -- | Send a `msg` to the whole hydra network.
    broadcast :: msg -> m ()
  }

instance Contravariant (HydraNetwork m) where
  contramap f (HydraNetwork bcast) = HydraNetwork $ \msg -> bcast (f msg)

-- |Handle to interface for inbound messages.
type NetworkCallback msg m = msg -> m ()

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
deriving anyclass instance Serialise tx => Serialise (HydraMessage tx)

instance ToCBOR tx => ToCBOR (HydraMessage tx) where
  toCBOR = \case
    ReqTx tx -> toCBOR ("ReqTx" :: Text) <> toCBOR tx
    AckTx party tx -> toCBOR ("AckTx" :: Text) <> toCBOR party <> toCBOR tx
    ConfTx -> toCBOR ("ConfTx" :: Text)
    ReqSn -> toCBOR ("ReqSn" :: Text)
    AckSn -> toCBOR ("AckSn" :: Text)
    ConfSn -> toCBOR ("ConfSn" :: Text)
    Ping pty -> toCBOR ("ConfSn" :: Text) <> toCBOR pty

instance FromCBOR tx => FromCBOR (HydraMessage tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "AckTx" -> AckTx <$> fromCBOR <*> fromCBOR
      "ConfTx" -> pure ConfTx
      "ReqSn" -> pure ReqSn
      "AckSn" -> pure AckSn
      "ConfSn" -> pure ConfSn
      "Ping" -> Ping <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded HydraMessage"
