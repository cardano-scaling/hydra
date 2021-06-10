module Hydra.Network.BroadcastToSelf where

import Cardano.Prelude
import Control.Monad.Class.MonadThrow (MonadThrow)
import Hydra.Network (HydraNetwork (..), NetworkCallback, NetworkComponent)

withBroadcastToSelf ::
  MonadThrow m =>
  NetworkComponent m msg ->
  NetworkComponent m msg
withBroadcastToSelf withNetwork callback action =
  withNetwork callback $ \network -> action (sendbackMessages network callback)

sendbackMessages ::
  MonadThrow m =>
  HydraNetwork m msg ->
  NetworkCallback msg m ->
  HydraNetwork m msg
sendbackMessages HydraNetwork{broadcast} callback =
  HydraNetwork{broadcast = \msg -> broadcast msg >> callback msg}
