module Hydra.Network.AuthenticateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Hydra.Network (Network (..), NetworkComponent, NodeId (..))
import Hydra.Network.HeartbeatSpec (noop)
import Test.Hydra.Fixture (alice, bob)
import Hydra.Crypto (Signature)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

      nodeId = NodeId "node_id-1"

      otherNodeId = NodeId "node_id-2"

  fit "pass the authenticated messages around" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Integer])

          withAuthentication nodeId (\incoming _ -> incoming (Authenticated 1)) (captureIncoming receivedMessages) $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [1]

  fit "drop unauthenticated messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Integer])

          withAuthentication nodeId (\incoming _ -> incoming (Authenticated 1) >> incoming (Authenticated 2)) (captureIncoming receivedMessages) $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [1]

  fit "authenticate the message to broadcast" $ do
    let someMessage = 1
        sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Authenticated Integer])

          withAuthentication nodeId (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 1

          readTVarIO sentMessages

    sentMsgs `shouldBe` [Authenticated 1]


data Authenticated msg = Authenticated {payload :: msg, signature :: Signature msg} deriving (Eq, Show)

withAuthentication ::
  ( MonadAsync m
  , MonadDelay m
  , MonadMonotonicTime m
  ) =>
  NodeId ->
  NetworkComponent m (Authenticated msg) a ->
  NetworkComponent m msg a
withAuthentication nodeId withRawNetwork callback action = do
  withRawNetwork verify sign
 where
  verify (Authenticated msg _sig) = callback msg
  sign = action . contramap Authenticated



