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

  -- * Simulated Network
  createSimulatedHydraNetwork,

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
import Control.Monad.Class.MonadSTM (MonadSTM (STM))
import Data.IP (IP)
import qualified Data.List as List
import Data.String (String)
import Hydra.Logic (HydraMessage (..), NetworkEvent (MessageReceived))
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
    AckTx -> toCBOR ("AckTx" :: Text)
    ConfTx -> toCBOR ("ConfTx" :: Text)
    ReqSn -> toCBOR ("ReqSn" :: Text)
    AckSn -> toCBOR ("AckSn" :: Text)
    ConfSn -> toCBOR ("ConfSn" :: Text)

instance FromCBOR tx => FromCBOR (HydraMessage tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "AckTx" -> pure AckTx
      "ConfTx" -> pure ConfTx
      "ReqSn" -> pure ReqSn
      "AckSn" -> pure AckSn
      "ConfSn" -> pure ConfSn
      msg -> fail $ show msg <> " is not a proper CBOR-encoded HydraMessage"

-- | A dummy implemenation for stubbing purpose
createSimulatedHydraNetwork ::
  Show tx => [Host] -> NetworkCallback tx IO -> IO (HydraNetwork tx IO)
createSimulatedHydraNetwork _ callback =
  pure HydraNetwork{broadcast = simulatedBroadcast}
 where
  simulatedBroadcast msg = do
    putText $ "[Network] should broadcast " <> show msg
    let ma = case msg of
          ReqTx _ -> Just AckTx
          AckTx -> Just ConfTx
          ConfTx -> Nothing
          ReqSn -> Just AckSn
          AckSn -> Just ConfSn
          ConfSn -> Nothing
    case ma of
      Just answer -> do
        putText $ "[Network] simulating answer " <> show answer
        callback (MessageReceived answer)
      Nothing -> pure ()
