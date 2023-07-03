
module Hydra.Network.AuthenticateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow, IOSim)
import Hydra.Network (Network (..), NodeId (..), NetworkComponent)
import Test.Hydra.Fixture (alice, bob)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

      nodeId = NodeId "node_id-1"

      otherNodeId = NodeId "node_id-2"

  it "pass the messages around" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Integer])

          withAuthentication nodeId (\incoming _ -> incoming (Authenticated 1)) (captureIncoming receivedMessages) $ \_ ->
            threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [1]

newtype Authenticated msg = Authenticated { payload ::  msg }

withAuthentication ::
  ( MonadAsync m
  , MonadDelay m
  , MonadMonotonicTime m
  ) =>
  NodeId ->
  NetworkComponent m (Authenticated msg) a ->
  NetworkComponent m msg a
withAuthentication = undefined



