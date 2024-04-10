module Hydra.Network.HeartbeatSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (Network (..), NetworkComponent, NodeId (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Message (Connectivity (Connected, Disconnected))

spec :: Spec
spec = parallel $ do
  let nodeId = NodeId "node_id-1"

      otherNodeId = NodeId "node_id-2"

  it "sends a heartbeat message with local host after 500 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          (capturingComponent, getOutgoingMessages) <- captureOutgoing

          withHeartbeat nodeId capturingComponent noop $ \_ ->
            threadDelay 1.1

          getOutgoingMessages

    sentHeartbeats `shouldBe` [Ping nodeId]

  it "sends Connected when Ping received from other peer" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          withHeartbeat nodeId (\incoming _ -> incoming (Ping otherNodeId)) callback $ \_ ->
            threadDelay 1

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Connected otherNodeId]

  it "sends Connected when any message received from other party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          withHeartbeat nodeId (\incoming _ -> incoming (Data otherNodeId ())) callback $ \_ ->
            threadDelay 1

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Connected otherNodeId]

  it "do not send Connected on subsequent messages from already Connected party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          withHeartbeat nodeId (\incoming _ -> incoming (Data otherNodeId ()) >> incoming (Ping otherNodeId)) callback $ \_ ->
            threadDelay 1

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Connected otherNodeId]

  it "sends Disconnected given no messages has been received from known party within twice heartbeat delay" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          let component incoming action =
                race_
                  (action (Network noop))
                  (incoming (Ping otherNodeId) >> threadDelay 4 >> incoming (Ping otherNodeId) >> threadDelay 7)

          withHeartbeat nodeId component callback $ \_ ->
            threadDelay 20

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Disconnected otherNodeId, Connected otherNodeId]

  it "stop sending heartbeat message given action sends a message" $ do
    let sentHeartbeats = runSimOrThrow $ do
          (capturingComponent, getOutgoingMessages) <- captureOutgoing

          withHeartbeat nodeId capturingComponent noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast ()
            threadDelay 1

          getOutgoingMessages

    sentHeartbeats `shouldBe` [Data nodeId (), Ping nodeId]

  it "restart sending heartbeat messages given last message sent is older than heartbeat delay" $ do
    let sentHeartbeats = runSimOrThrow $ do
          (capturingComponent, getOutgoingMessages) <- captureOutgoing

          withHeartbeat nodeId capturingComponent noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast ()
            threadDelay 3.6

          getOutgoingMessages

    sentHeartbeats `shouldBe` [Ping nodeId, Data nodeId (), Ping nodeId]

noop :: Monad m => b -> m ()
noop = const $ pure ()

captureOutgoing :: MonadSTM m => m (NetworkComponent m (Heartbeat ()) (Heartbeat ()) (), m [Heartbeat ()])
captureOutgoing = do
  tv <- newTVarIO []
  pure (\_ action -> action Network{broadcast = broadcast tv}, readTVarIO tv)
 where
  broadcast tv msg =
    atomically $ modifyTVar' tv (msg :)

captureConnectivity :: MonadSTM m => m (Either Connectivity a -> m (), m [Connectivity])
captureConnectivity = do
  tv <- newTVarIO []
  pure (record tv, readTVarIO tv)
 where
  record tv = \case
    Left c -> atomically $ modifyTVar' tv (c :)
    Right _ -> pure ()
