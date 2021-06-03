{-# LANGUAGE TypeApplications #-}

module Hydra.Network.Heartbeat where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Prelude
import Control.Monad (fail)
import Data.Functor.Contravariant (contramap)
import Hydra.Ledger (Party)
import Hydra.Logic (HydraMessage, NetworkEvent)
import Hydra.Network (Host, HydraNetwork (..), NetworkCallback)

data HeartbeatMessage tx
  = HydraMessage (HydraMessage tx)
  | Heartbeat Party
  deriving (Eq, Show)

instance ToCBOR tx => ToCBOR (HeartbeatMessage tx) where
  toCBOR = \case
    HydraMessage msg -> toCBOR (0 :: Word8) <> toCBOR msg
    Heartbeat party -> toCBOR (1 :: Word8) <> toCBOR party

instance FromCBOR tx => FromCBOR (HeartbeatMessage tx) where
  fromCBOR =
    fromCBOR @Word8 >>= \case
      0 -> HydraMessage <$> fromCBOR
      1 -> Heartbeat <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded HydraMessage"

withHeartbeat ::
  [Host] ->
  (NetworkCallback (HeartbeatMessage tx) m -> (HydraNetwork m (HeartbeatMessage tx) -> m ()) -> m ()) ->
  NetworkCallback (NetworkEvent tx) m ->
  (HydraNetwork m (HydraMessage tx) -> m ()) ->
  m ()
withHeartbeat _peers withNetwork callback action =
  withNetwork (callback . wrap) (action . contramap HydraMessage)

wrap :: HeartbeatMessage tx -> NetworkEvent tx
wrap = panic "not implemented"
