module Hydra.Network.HeartbeatSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (Network (..), NodeId (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Message (Connectivity (Connected, Disconnected))

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      nodeId = NodeId "node_id-1"

      otherNodeId = NodeId "node_id-2"

  it "sends a heartbeat message with local host after 500 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Heartbeat ()])

          withHeartbeat nodeId (captureOutgoing sentMessages) noop $ \_ ->
            threadDelay 1.1

          readTVarIO sentMessages

    sentHeartbeats `shouldBe` [Ping nodeId]

  it "sends Connected when Ping received from other peer" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Connectivity])

          withHeartbeat nodeId (\incoming _ -> incoming (Ping otherNodeId)) noop $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [Connected otherNodeId]

  it "sends Connected when any message received from other party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Connectivity])

          withHeartbeat nodeId (\incoming _ -> incoming (Data otherNodeId ())) noop $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [Connected otherNodeId]

  it "do not send Connected on subsequent messages from already Connected party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Connectivity])

          withHeartbeat nodeId (\incoming _ -> incoming (Data otherNodeId ()) >> incoming (Ping otherNodeId)) noop $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [Connected otherNodeId]

  it "sends Disconnected given no messages has been received from known party within twice heartbeat delay" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Connectivity])

          let component incoming action =
                race_
                  (action (Network noop))
                  (incoming (Ping otherNodeId) >> threadDelay 4 >> incoming (Ping otherNodeId) >> threadDelay 7)

          withHeartbeat nodeId component noop $ \_ ->
            threadDelay 20

          readTVarIO receivedMessages

    receivedHeartbeats `shouldBe` [Disconnected otherNodeId, Connected otherNodeId]

  it "stop sending heartbeat message given action sends a message" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Heartbeat ()])

          withHeartbeat nodeId (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast ()
            threadDelay 1

          readTVarIO sentMessages

    sentHeartbeats `shouldBe` [Data nodeId (), Ping nodeId]

  it "restart sending heartbeat messages given last message sent is older than heartbeat delay" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Heartbeat ()])

          withHeartbeat nodeId (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast ()
            threadDelay 3.6

          readTVarIO sentMessages

    sentHeartbeats `shouldBe` [Ping nodeId, Data nodeId (), Ping nodeId]

noop :: Monad m => b -> m ()
noop = const $ pure ()
