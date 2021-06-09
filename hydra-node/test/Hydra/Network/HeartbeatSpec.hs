{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Network.HeartbeatSpec where

import Cardano.Prelude hiding (atomically, threadDelay)
import Control.Monad.Class.MonadSTM (atomically, modifyTVar', newTVarIO, readTVar)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Logic (HydraMessage (AckTx, Ping))
import Hydra.Network (HydraNetwork (..))
import Hydra.Network.Heartbeat (HeartbeatMessage (Heartbeat, HydraMessage), withHeartbeat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Heartbeat" $ do
  it "sends a heartbeat message with own party id every 100 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [HeartbeatMessage Integer])
          let broadcast msg = atomically $ modifyTVar' sentMessages (msg :)
              dummyNetwork _cb action = action $ HydraNetwork{..}

          withHeartbeat 1 dummyNetwork noop $ \_ -> do
            threadDelay 1.1

          atomically $ readTVar sentMessages

    sentHeartbeats `shouldBe` replicate 10 (Heartbeat 1)

  it "propagates Heartbeat received from other parties" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [HydraMessage Integer])

          let dummyNetwork cb action = action (HydraNetwork noop) >> cb (Heartbeat 2)
              receive msg = atomically $ modifyTVar' receivedMessages (msg :)

          withHeartbeat 1 dummyNetwork receive $ \_ -> do
            threadDelay 1

          atomically $ readTVar receivedMessages

    receivedHeartbeats `shouldBe` [Ping 2]

  it "stop sending heartbeat message given action sends a message" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [HeartbeatMessage Integer])
          let dummyNetwork _cb action = action $ HydraNetwork{broadcast = \msg -> atomically $ modifyTVar' sentMessages (msg :)}

          withHeartbeat 1 dummyNetwork noop $ \HydraNetwork{broadcast} -> do
            threadDelay 0.5
            broadcast AckTx
            threadDelay 1

          atomically $ readTVar sentMessages

    sentHeartbeats `shouldBe` (HydraMessage AckTx : replicate 4 (Heartbeat 1))

noop :: Monad m => b -> m ()
noop = const $ pure ()
