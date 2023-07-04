{-# LANGUAGE TypeApplications #-}

module Hydra.Network.AuthenticateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Data.ByteString (pack)
import Hydra.Crypto (sign)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (Authenticated), withAuthentication)
import Hydra.Network.HeartbeatSpec (noop)
import Hydra.NetworkSpec (prop_canRoundtripCBOREncoding)
import Test.Hydra.Fixture (aliceSk, bob, bobSk)
import Test.QuickCheck (generate, listOf)

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

  it "drop message coming from unknown party" $ do
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

  it "drop message comming from party with wrong signature" $
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [ByteString])

          withAuthentication
            aliceSk
            [bob, carol]
            ( \incoming _ -> do
                incoming (Authenticated "1" (sign carolSk "1") bob)
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

  describe "Serialization" $ do
    prop "can roundtrip CBOR encoding/decoding of Authenticated Hydra Message" $
      prop_canRoundtripCBOREncoding @(Authenticated Msg)


newtype Msg = Msg ByteString
  deriving newtype (Eq, Show, ToCBOR, FromCBOR, SignableRepresentation)

instance Arbitrary Msg where
  arbitrary = Msg . pack <$> listOf arbitrary

