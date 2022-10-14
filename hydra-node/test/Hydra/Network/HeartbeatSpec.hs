module Hydra.Network.HeartbeatSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (HydraNodeId (HydraNodeId), Network (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Message (Message (Connected, Disconnected, ReqTx))
import Test.Hydra.Fixture (alice, bob)

spec :: Spec
spec = parallel $
  describe "Heartbeat" $ do
    let captureOutgoing msgqueue _cb action =
          action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

        captureIncoming receivedMessages msg =
          atomically $ modifyTVar' receivedMessages (msg :)

        nodeId = HydraNodeId "node_id-1"

        otherNodeId = HydraNodeId "node_id-2"

    it "sends a heartbeat message with local host after 500 ms" $ do
      let sentHeartbeats = runSimOrThrow $ do
            sentMessages <- newTVarIO ([] :: [Heartbeat (Message Integer)])

            withHeartbeat nodeId (captureOutgoing sentMessages) noop $ \_ ->
              threadDelay 1.1

            readTVarIO sentMessages

      sentHeartbeats `shouldBe` [Ping nodeId]

    it "sends Connected when Ping received from other peer" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            withHeartbeat nodeId (\incoming _ -> incoming (Ping otherNodeId)) (captureIncoming receivedMessages) $ \_ ->
              threadDelay 1

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [Connected otherNodeId]

    it "sends Connected when any message received from other party" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            withHeartbeat nodeId (\incoming _ -> incoming (Data otherNodeId $ ReqTx bob 1)) (captureIncoming receivedMessages) $ \_ ->
              threadDelay 1

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [ReqTx bob 1, Connected otherNodeId]

    it "do not send Connected on subsequent messages from already Connected party" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            withHeartbeat nodeId (\incoming _ -> incoming (Data otherNodeId $ ReqTx bob 1) >> incoming (Ping otherNodeId)) (captureIncoming receivedMessages) $ \_ ->
              threadDelay 1

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [ReqTx bob 1, Connected otherNodeId]

    it "sends Disconnected given no messages has been received from known party within twice heartbeat delay" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            let component incoming action =
                  race_
                    (action (Network noop))
                    (incoming (Ping otherNodeId) >> threadDelay 4 >> incoming (Ping otherNodeId) >> threadDelay 7)

            withHeartbeat nodeId component (captureIncoming receivedMessages) $ \_ ->
              threadDelay 20

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [Disconnected otherNodeId, Connected otherNodeId]

    it "stop sending heartbeat message given action sends a message" $ do
      let someMessage = ReqTx alice 1
          sentHeartbeats = runSimOrThrow $ do
            sentMessages <- newTVarIO ([] :: [Heartbeat (Message Integer)])

            withHeartbeat nodeId (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
              threadDelay 0.6
              broadcast someMessage
              threadDelay 1

            readTVarIO sentMessages

      sentHeartbeats `shouldBe` [Data nodeId someMessage, Ping nodeId]

    it "restart sending heartbeat messages given last message sent is older than heartbeat delay" $ do
      let someMessage = ReqTx alice 1
          sentHeartbeats = runSimOrThrow $ do
            sentMessages <- newTVarIO ([] :: [Heartbeat (Message Integer)])

            withHeartbeat nodeId (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
              threadDelay 0.6
              broadcast someMessage
              threadDelay 3.6

            readTVarIO sentMessages

      sentHeartbeats `shouldBe` [Ping nodeId, Data nodeId someMessage, Ping nodeId]

noop :: Monad m => b -> m ()
noop = const $ pure ()
