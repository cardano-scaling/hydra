{-# LANGUAGE TypeApplications #-}

module Hydra.Network.Heartbeat where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Prelude hiding (threadDelay, withAsync)
import Control.Monad (fail)
import Control.Monad.Class.MonadAsync (MonadAsync, withAsync)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.Functor.Contravariant (contramap)
import Hydra.Ledger (Party)
import Hydra.Logic (HydraMessage (..))
import Hydra.Network (HydraNetwork (..), NetworkCallback)

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
  (MonadAsync m, MonadDelay m) =>
  Party ->
  (NetworkCallback (HeartbeatMessage tx) m -> (HydraNetwork m (HeartbeatMessage tx) -> m ()) -> m ()) ->
  NetworkCallback (HydraMessage tx) m ->
  (HydraNetwork m (HydraMessage tx) -> m ()) ->
  m ()
withHeartbeat me withNetwork callback action =
  withNetwork (callback . wrap) $ \network ->
    withAsync (sendHeartbeatFor me network) $ \_ ->
      action (contramap HydraMessage network)

sendHeartbeatFor ::
  MonadDelay m =>
  Party ->
  HydraNetwork m (HeartbeatMessage tx) ->
  m ()
sendHeartbeatFor me HydraNetwork{broadcast} =
  forever $ threadDelay 0.1 >> broadcast (Heartbeat me)

wrap :: HeartbeatMessage tx -> HydraMessage tx
wrap = panic "not implemented"
