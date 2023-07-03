module Hydra.Network.AuthenticateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Crypto (sign)
import Hydra.Network (Network (..))
import Hydra.Network.HeartbeatSpec (noop)
import Test.Hydra.Fixture (aliceSk, bob, bobSk)
import Test.QuickCheck (generate)
import Hydra.Network.Authenticate (Authenticated (Authenticated), withAuthentication)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

  it "pass the authenticated messages around" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [ByteString])

          withAuthentication
            aliceSk
            [bob]
            ( \incoming _ -> do
                incoming (Authenticated "1" (sign bobSk "1"))
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` ["1"]

  it "drop unauthenticated messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [ByteString])

          withAuthentication
            aliceSk
            [bob]
            ( \incoming _ -> do
                incoming (Authenticated "1" (sign bobSk "1"))
                incoming (Authenticated "2" (sign aliceSk "2"))
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` ["1"]

  it "authenticate the message to broadcast" $ do
    signingKey <- generate arbitrary
    let someMessage = "1"
        sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Authenticated ByteString])

          withAuthentication signingKey [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 1

          readTVarIO sentMessages

    sentMsgs `shouldBe` [Authenticated "1" (sign signingKey "1")]

