module Hydra.Network.HeartbeatSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (Connectivity (..), Host (..), Network (..), NetworkCallback (..), NetworkComponent)
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)

spec :: Spec
spec = parallel $ do
  let localHost = Host "192.168.0.123" 5001

      otherHost = Host "192.168.0.111" 1234

  it "sends a heartbeat message with local host after 500 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          (capturingComponent, getOutgoingMessages) <- captureOutgoing

          withHeartbeat localHost capturingComponent noop $ \_ ->
            threadDelay 1.1

          getOutgoingMessages

    sentHeartbeats `shouldBe` [Ping localHost]

  it "sends Connected when Ping received from other peer" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          withHeartbeat localHost (\NetworkCallback{deliver} _ -> deliver (Ping otherHost)) callback $ \_ ->
            threadDelay 1

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Connected otherHost]

  it "sends Connected when any message received from other party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          withHeartbeat localHost (\NetworkCallback{deliver} _ -> deliver (Data otherHost ())) callback $ \_ ->
            threadDelay 1

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Connected otherHost]

  it "do not send Connected on subsequent messages from already Connected party" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          withHeartbeat localHost (\NetworkCallback{deliver} _ -> deliver (Data otherHost ()) >> deliver (Ping otherHost)) callback $ \_ ->
            threadDelay 1

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Connected otherHost]

  it "sends Disconnected given no messages has been received from known party within twice heartbeat delay" $ do
    let receivedHeartbeats = runSimOrThrow $ do
          (callback, getConnectivityEvents) <- captureConnectivity

          let component NetworkCallback{deliver} action =
                race_
                  (action (Network{broadcast = const $ pure ()}))
                  (deliver (Ping otherHost) >> threadDelay 4 >> deliver (Ping otherHost) >> threadDelay 7)

          withHeartbeat localHost component callback $ \_ ->
            threadDelay 20

          getConnectivityEvents

    receivedHeartbeats `shouldBe` [Disconnected otherHost, Connected otherHost]

  it "stop sending heartbeat message given action sends a message" $ do
    let sentHeartbeats = runSimOrThrow $ do
          (capturingComponent, getOutgoingMessages) <- captureOutgoing

          withHeartbeat localHost capturingComponent noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast ()
            threadDelay 1

          getOutgoingMessages

    sentHeartbeats `shouldBe` [Data localHost (), Ping localHost]

  it "restart sending heartbeat messages given last message sent is older than heartbeat delay" $ do
    let sentHeartbeats = runSimOrThrow $ do
          (capturingComponent, getOutgoingMessages) <- captureOutgoing

          withHeartbeat localHost capturingComponent noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast ()
            threadDelay 3.6

          getOutgoingMessages

    sentHeartbeats `shouldBe` [Ping localHost, Data localHost (), Ping localHost]

noop :: Monad m => NetworkCallback b m
noop =
  NetworkCallback
    { deliver = const $ pure ()
    , onConnectivity = const $ pure ()
    }

captureOutgoing :: MonadSTM m => m (NetworkComponent m (Heartbeat ()) (Heartbeat ()) (), m [Heartbeat ()])
captureOutgoing = do
  tv <- newTVarIO []
  pure (\_ action -> action Network{broadcast = broadcast tv}, readTVarIO tv)
 where
  broadcast tv msg =
    atomically $ modifyTVar' tv (msg :)

captureConnectivity :: MonadSTM m => m (NetworkCallback a m, m [Connectivity])
captureConnectivity = do
  tv <- newTVarIO []
  pure (NetworkCallback{deliver = const $ pure (), onConnectivity = record tv}, readTVarIO tv)
 where
  record tv c = atomically $ modifyTVar' tv (c :)
