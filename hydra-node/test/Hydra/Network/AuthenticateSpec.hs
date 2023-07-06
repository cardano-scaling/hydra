{-# LANGUAGE TypeApplications #-}

module Hydra.Network.AuthenticateSpec where

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Data.ByteString (pack)
import Hydra.Crypto (sign)
import Hydra.Logging (Envelope (message), nullTracer, traceInTVar)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (AuthLog (AuthLog), Authenticated (..), Signed (Signed), withAuthentication)
import Hydra.Network.HeartbeatSpec (noop)
import Hydra.NetworkSpec (prop_canRoundtripCBOREncoding)
import Hydra.Prelude
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.Hydra.Prelude
import Test.QuickCheck (listOf)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

  it "pass the authenticated messages around" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Authenticated ByteString])

          withAuthentication
            nullTracer
            aliceSk
            [bob]
            ( \incoming _ -> do
                incoming (Signed "1" (sign bobSk "1") bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated "1" bob]

  it "drop message coming from unknown party" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Authenticated ByteString])

          withAuthentication
            nullTracer
            aliceSk
            [bob]
            ( \incoming _ -> do
                incoming (Signed "1" (sign bobSk "1") bob)
                incoming (Signed "2" (sign aliceSk "2") alice)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated "1" bob]

  it "drop message coming from party with wrong signature" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Authenticated ByteString])

          withAuthentication
            nullTracer
            aliceSk
            [bob, carol]
            ( \incoming _ -> do
                incoming (Signed "1" (sign carolSk "1") bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` []

  it "authenticate the message to broadcast" $ do
    let someMessage = Authenticated "1" bob
        sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Signed ByteString])

          withAuthentication nullTracer bobSk [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 1

          readTVarIO sentMessages

    sentMsgs `shouldBe` [Signed "1" (sign bobSk "1") bob]

  it "logs dropped messages" $ do
    let traced = runSimOrThrow $ do
          traces <- newTVarIO []
          let tracer = traceInTVar traces
          withAuthentication @_ @ByteString tracer aliceSk [bob, carol] (\incoming _ -> incoming (Signed "1" (sign carolSk "1") bob)) noop $ \_ ->
            threadDelay 1

          readTVarIO traces

    (message <$> traced) `shouldContain` [AuthLog]

  describe "Serialization" $ do
    prop "can roundtrip CBOR encoding/decoding of Signed Hydra Message" $
      prop_canRoundtripCBOREncoding @(Signed Msg)

newtype Msg = Msg ByteString
  deriving newtype (Eq, Show, ToCBOR, FromCBOR, SignableRepresentation)

instance Arbitrary Msg where
  arbitrary = Msg . pack <$> listOf arbitrary
