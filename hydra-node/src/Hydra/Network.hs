{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |Interface to the Hydra network and base types
-- Concrete implementations are provided by submodules. Import those instead of this one
-- if interested in actually configuring and running a real network layer.
module Hydra.Network where

import Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Prelude
import Codec.Serialise
import Hydra.Logic (HydraMessage (..))
import Network.Socket (HostName, ServiceName)
import Network.TypedProtocol.Pipelined ()

type Host = (HostName, Port)

type Port = ServiceName

--
-- HydraNetwork handle to abstract over network access
--

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage -> m ()
  }

type NetworkCallback m = HydraMessage -> m ()

--
-- Concrete network implementations
--

deriving stock instance Generic HydraMessage
deriving anyclass instance Serialise HydraMessage

instance ToCBOR HydraMessage where
  toCBOR = \case
    ReqTx -> toCBOR ("ReqTx" :: Text)
    AckTx -> toCBOR ("AckTx" :: Text)
    ConfTx -> toCBOR ("ConfTx" :: Text)
    ReqSn -> toCBOR ("ReqSn" :: Text)
    AckSn -> toCBOR ("AckSn" :: Text)
    ConfSn -> toCBOR ("ConfSn" :: Text)

instance FromCBOR HydraMessage where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> pure ReqTx
      _ -> panic "TODO: fromCBOR HydraMessage"

-- | A dummy implemenation for stubbing purpose
createSimulatedHydraNetwork :: [Host] -> NetworkCallback IO -> IO (HydraNetwork IO)
createSimulatedHydraNetwork _ callback =
  pure HydraNetwork{broadcast = simulatedBroadcast}
 where
  simulatedBroadcast msg = do
    putText $ "[Network] should broadcast " <> show msg
    let ma = case msg of
          ReqTx -> Just AckTx
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
