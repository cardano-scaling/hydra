module Hydra.FireForgetSpec where

import Cardano.Prelude
import Control.Monad.IOSim (runSimOrThrow)
import Network.Mux.Channel (createBufferConnectedChannels)
import Network.TypedProtocol.Driver.Simple (runPeer)
import Test.Hspec

spec :: Spec
spec = describe "Fire-Forget Ouroboros Protocol" $ do
  it "client can send 'Hail Hydra!' to server" $ do
    let tracer = panic "undefined"
        action = do
          (channelA, channelB) <- createBufferConnectedChannels
          concurrently
            (runPeer tracer codecFireForget channelA serverPeer)
            (runPeer tracer codecFireForget channelB clientPeer)

    pending
