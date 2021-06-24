module Hydra.Network.HeartbeatSpec where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO, readTVar)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.HeadLogic (HydraMessage (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Network (Host (..), Network (..))
import Hydra.Network.Heartbeat (withHeartbeat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Heartbeat" $ do
  let dummyNetwork msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

  it "sends a heartbeat message with own party id every 500 ms" $ do
    let sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [HydraMessage SimpleTx])

          withHeartbeat testHost (dummyNetwork sentMessages) noop $ \_ -> do
            threadDelay 1.1

          atomically $ readTVar sentMessages

    sentHeartbeats `shouldBe` replicate 2 (Ping testHost)

  it "propagates Heartbeat received from other parties" $ do
    let anotherHost = Host{hostName = "0.0.0.0", portNumber = 4001}
    let receivedHeartbeats = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [HydraMessage SimpleTx])

          let receive msg = atomically $ modifyTVar' receivedMessages (msg :)
          withHeartbeat testHost (\cb action -> action (Network noop) >> cb (Ping anotherHost)) receive $ \_ -> do
            threadDelay 1

          atomically $ readTVar receivedMessages

    receivedHeartbeats `shouldBe` [Ping anotherHost]

  it "stop sending heartbeat message given action sends a message" $ do
    let someMessage = ReqTx 1
        sentHeartbeats = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [HydraMessage Integer])

          withHeartbeat testHost (dummyNetwork sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 1

          atomically $ readTVar sentMessages

    sentHeartbeats `shouldBe` [someMessage, Ping testHost]

testHost :: Host
testHost = Host{hostName = "0.0.0.0", portNumber = 4000}

noop :: Monad m => b -> m ()
noop = const $ pure ()
