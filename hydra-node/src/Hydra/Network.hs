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
import Data.IP (IP)
import qualified Data.List as List
import Data.String (String)
import Hydra.HeadLogic (HydraMessage (..), NetworkEvent (..))
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol.Pipelined ()

-- * Hydra network interface

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork tx m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage tx -> m ()
  }

type NetworkCallback tx m = NetworkEvent tx -> m ()

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
    ReqSn txs -> toCBOR ("ReqSn" :: Text) <> toCBOR txs
    AckSn -> toCBOR ("AckSn" :: Text)
    ConfSn -> toCBOR ("ConfSn" :: Text)

instance FromCBOR tx => FromCBOR (HydraMessage tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "AckTx" -> AckTx <$> fromCBOR <*> fromCBOR
      "ConfTx" -> pure ConfTx
      "ReqSn" -> ReqSn <$> fromCBOR
      "AckSn" -> pure AckSn
      "ConfSn" -> pure ConfSn
      msg -> fail $ show msg <> " is not a proper CBOR-encoded HydraMessage"
