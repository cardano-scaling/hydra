module Hydra.Network.ZeroMQ where

import Cardano.Prelude
import Hydra.Network

withZeroMQHydraNetwork ::
  Host ->
  [Host] ->
  NetworkCallback IO ->
  (HydraNetwork IO -> IO ()) ->
  IO ()
withZeroMQHydraNetwork _localHost _remoteHosts _networkCallback _between =
  panic "TODO"
