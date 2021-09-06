module Hydra.Network.HeartbeatSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (Host (..), Network (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Message (Message (Connected, Disconnected, ReqTx))

spec :: Spec
spec = parallel $
  describe "Heartbeat" $ do
    let captureOutgoing msgqueue _cb action =
          action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

        captureIncoming receivedMessages msg =
          atomically $ modifyTVar' receivedMessages (msg :)

        localhost = Host{hostname = "1.2.3.4", port = 1}

        otherPeer = Host{hostname = "2.3.4.5", port = 1}

    it "sends a heartbeat message with local host after 500 ms" $ do
      let sentHeartbeats = runSimOrThrow $ do
            sentMessages <- newTVarIO ([] :: [Heartbeat (Message Integer)])

            withHeartbeat localhost (captureOutgoing sentMessages) noop $ \_ ->
              threadDelay 1.1

            readTVarIO sentMessages

      sentHeartbeats `shouldBe` [Ping localhost]

    it "sends Connected when Ping received from other peer" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            withHeartbeat localhost (\incoming _ -> incoming (Ping otherPeer)) (captureIncoming receivedMessages) $ \_ ->
              threadDelay 1

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [Connected otherPeer]

    it "sends Connected when any message received from other party" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            withHeartbeat localhost (\incoming _ -> incoming (Data otherPeer $ ReqTx 2 1)) (captureIncoming receivedMessages) $ \_ ->
              threadDelay 1

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [ReqTx 2 1, Connected otherPeer]

    it "do not send Connected on subsequent messages from already Connected party" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            withHeartbeat localhost (\incoming _ -> incoming (Data otherPeer $ ReqTx 2 1) >> incoming (Ping otherPeer)) (captureIncoming receivedMessages) $ \_ ->
              threadDelay 1

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [ReqTx 2 1, Connected otherPeer]

    it "sends Disconnected given no messages has been received from known party within twice heartbeat delay" $ do
      let receivedHeartbeats = runSimOrThrow $ do
            receivedMessages <- newTVarIO ([] :: [Message Integer])

            let component incoming action =
                  race_
                    (action (Network noop))
                    (incoming (Ping otherPeer) >> threadDelay 4 >> incoming (Ping otherPeer) >> threadDelay 7)

            withHeartbeat localhost component (captureIncoming receivedMessages) $ \_ ->
              threadDelay 20

            readTVarIO receivedMessages

      receivedHeartbeats `shouldBe` [Disconnected otherPeer, Connected otherPeer]

    it "stop sending heartbeat message given action sends a message" $ do
      let someMessage = ReqTx 1 1
          sentHeartbeats = runSimOrThrow $ do
            sentMessages <- newTVarIO ([] :: [Heartbeat (Message Integer)])

            withHeartbeat localhost (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
              threadDelay 0.6
              broadcast someMessage
              threadDelay 1

            readTVarIO sentMessages

      sentHeartbeats `shouldBe` [Data localhost someMessage, Ping localhost]

    it "restart sending heartbeat messages given last message sent is older than heartbeat delay" $ do
      let someMessage = ReqTx 1 1
          sentHeartbeats = runSimOrThrow $ do
            sentMessages <- newTVarIO ([] :: [Heartbeat (Message Integer)])

            withHeartbeat localhost (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
              threadDelay 0.6
              broadcast someMessage
              threadDelay 3.6

            readTVarIO sentMessages

      sentHeartbeats `shouldBe` [Ping localhost, Data localhost someMessage, Ping localhost]

testHost :: Host
testHost = Host{hostname = "0.0.0.0", port = 4000}

noop :: Monad m => b -> m ()
noop = const $ pure ()
