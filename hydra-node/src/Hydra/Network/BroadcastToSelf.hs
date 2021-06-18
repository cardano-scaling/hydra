module Hydra.Network.BroadcastToSelf where

import Hydra.Prelude

import Hydra.Network (Network (..), NetworkCallback, NetworkComponent)

-- | Ensures that messages `broadcast` through the wrapped `NetworkComponent` are
-- also sent back to "self" through passed `NetworkCallback`.
withBroadcastToSelf ::
  MonadThrow m =>
  NetworkComponent m msg ->
  NetworkComponent m msg
withBroadcastToSelf withNetwork callback action =
  withNetwork callback $ \network -> action (sendbackMessages network callback)

sendbackMessages ::
  MonadThrow m =>
  Network m msg ->
  NetworkCallback msg m ->
  Network m msg
sendbackMessages Network{broadcast} callback =
  Network{broadcast = \msg -> broadcast msg >> callback msg}
