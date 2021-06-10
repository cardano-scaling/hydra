{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Network.HeartbeatSpec where

import Cardano.Prelude hiding (atomically, threadDelay)
import Control.Monad.Class.MonadSTM (atomically, modifyTVar', newTVarIO, readTVar)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.HeadLogic (HydraMessage (..))
import Hydra.Network (HydraNetwork (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Heartbeat" $ do
  let dummyNetwork msgqueue _cb action =
        action $ HydraNetwork{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

  it "sends a heartbeat message with own party id every 500 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [HydraMessage Integer])

          withHeartbeat 1 (dummyNetwork sentMessages) noop $ \_ -> do
            threadDelay 1.1

          atomically $ readTVar sentMessages

    sentHeartbeats `shouldBe` replicate 2 (Ping 1)

  it "propagates Heartbeat received from other parties" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [HydraMessage Integer])

          let receive msg = atomically $ modifyTVar' receivedMessages (msg :)

          withHeartbeat 1 (\cb action -> action (HydraNetwork noop) >> cb (Ping 2)) receive $ \_ -> do
            threadDelay 1

          atomically $ readTVar receivedMessages

    receivedHeartbeats `shouldBe` [Ping 2]

  it "stop sending heartbeat message given action sends a message" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [HydraMessage Integer])

          withHeartbeat 1 (dummyNetwork sentMessages) noop $ \HydraNetwork{broadcast} -> do
            threadDelay 0.6
            broadcast ConfSn
            threadDelay 1

          atomically $ readTVar sentMessages

    sentHeartbeats `shouldBe` [ConfSn, Ping 1]

noop :: Monad m => b -> m ()
noop = const $ pure ()
