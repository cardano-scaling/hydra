{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Network.HeartbeatSpec where

import Cardano.Prelude hiding (atomically, threadDelay)
import Control.Monad.Class.MonadSTM (atomically, modifyTVar', newTVarIO, readTVar)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (HydraNetwork (..))
import Hydra.Network.Heartbeat (HeartbeatMessage (Heartbeat), withHeartbeat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Heartbeat" $ do
  it "sends a heartbeat message with own party id every 100 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [HeartbeatMessage Integer])
          let broadcast msg = atomically $ modifyTVar' sentMessages (msg :)
              dummyNetwork _cb action = action $ HydraNetwork{..}
              noop = const $ pure ()

          withHeartbeat 1 dummyNetwork noop $ \_ -> do
            threadDelay 1.1

          atomically $ readTVar sentMessages

    sentHeartbeats `shouldBe` replicate 10 (Heartbeat 1)
