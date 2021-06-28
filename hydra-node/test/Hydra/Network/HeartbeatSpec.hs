module Hydra.Network.HeartbeatSpec where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.HeadLogic (HydraMessage (..))
import Hydra.Network (
  Host (..),
  Network (..),
 )
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Heartbeat" $ do
  let dummyNetwork msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

  it "sends a heartbeat message with own party id after 500 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Heartbeat (HydraMessage Integer)])

          withHeartbeat 1 (dummyNetwork sentMessages) noop $ \_ ->
            threadDelay 1.1

          readTVarIO sentMessages

    sentHeartbeats `shouldBe` [Ping 1]

  it "sends Connected when Ping received from other party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [HydraMessage Integer])

          let receive msg = atomically $ modifyTVar' receivedMessages (msg :)
          withHeartbeat 1 (\cb action -> action (Network noop) >> cb (Ping 2)) receive $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [Connected 2]

  it "sends Connected when any message received from other party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [HydraMessage Integer])

          let receive msg = atomically $ modifyTVar' receivedMessages (msg :)
          withHeartbeat 1 (\cb action -> action (Network noop) >> cb (Message $ ReqTx 2 1)) receive $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [ReqTx 2 1, Connected 2]

  it "do not send Connected on subsequent messages from already Connected party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [HydraMessage Integer])

          let receive msg = atomically $ modifyTVar' receivedMessages (msg :)
          withHeartbeat 1 (\cb action -> action (Network noop) >> cb (Message $ ReqTx 2 1) >> cb (Ping 2)) receive $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [ReqTx 2 1, Connected 2]

  it "sends Disconnected given no messages has been received from known peer within 3s" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [HydraMessage Integer])

          let receive msg = atomically $ modifyTVar' receivedMessages (msg :)
          withHeartbeat 1 (\cb action -> race_ (action (Network noop)) (cb (Ping 2) >> threadDelay 4)) receive $ \_ ->
            threadDelay 10

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [Disconnected 2, Connected 2]

  it "stop sending heartbeat message given action sends a message" $ do
    let someMessage = ReqTx 1 1
        sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Heartbeat (HydraMessage Integer)])

          withHeartbeat 1 (dummyNetwork sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 1

          readTVarIO sentMessages

    sentHeartbeats `shouldBe` [Message someMessage, Ping 1]

  it "restart sending heartbeat messages given last message sent is older than 3s" $ do
    let someMessage = ReqTx 1 1
        sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Heartbeat (HydraMessage Integer)])

          withHeartbeat 1 (dummyNetwork sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 3.6

          readTVarIO sentMessages

    sentHeartbeats `shouldBe` [Ping 1, Message someMessage, Ping 1]

testHost :: Host
testHost = Host{hostName = "0.0.0.0", portNumber = 4000}

noop :: Monad m => b -> m ()
noop = const $ pure ()
