{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Interface to the Hydra network and base types. Concrete implementations are
-- provided by submodules. Import those instead of this one if interested in
-- actually configuring and running a real network layer.
module Hydra.Network where

import Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Prelude
import Codec.Serialise
import Control.Monad (fail)
import Hydra.Logic (HydraMessage (..))
import Network.Socket (HostName, ServiceName)
import Network.TypedProtocol.Pipelined ()

-- * Hydra network interface

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork tx m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage tx -> m ()
  }

type NetworkCallback tx m = HydraMessage tx -> m ()

-- * Types used by concrete implementations

type Host = (HostName, Port)

type Port = ServiceName

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
        callback answer
      Nothing -> pure ()
