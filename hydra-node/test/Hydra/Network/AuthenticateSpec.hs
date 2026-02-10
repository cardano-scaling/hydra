module Hydra.Network.AuthenticateSpec where

import Hydra.NetworkSpec (prop_canRoundtripCBOREncoding)
import "QuickCheck" Test.QuickCheck (listOf)
import "QuickCheck" Test.QuickCheck.Gen (generate)
import "bytestring" Data.ByteString (pack)
import "cardano-crypto-class" Cardano.Crypto.Util (SignableRepresentation)
import "hspec-golden-aeson" Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import "hydra-node" Hydra.Ledger.Simple (SimpleTx)
import "hydra-node" Hydra.Logging (Envelope (message), nullTracer, traceInTVar)
import "hydra-node" Hydra.Network (Network (..), NetworkCallback (..))
import "hydra-node" Hydra.Network.Authenticate (AuthLog, Authenticated (..), Signed (Signed), mkAuthLog, withAuthentication)
import "hydra-node" Hydra.Network.Message (Message (ReqTx))
import "hydra-node" Test.Hydra.Network.Authenticate ()
import "hydra-node" Test.Util (noopCallback)
import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Tx.Crypto (sign)
import "hydra-tx" Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import "io-classes" Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar')
import "io-sim" Control.Monad.IOSim (runSimOrThrow)

spec :: Spec
spec = parallel $ do
  let captureOutgoing :: MonadSTM m => TVar m [a] -> p -> (Network m a -> b) -> b
      captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming :: MonadSTM m => TVar m [mes] -> NetworkCallback mes m
      captureIncoming receivedMessages =
        NetworkCallback
          { deliver = \msg ->
              atomically $ modifyTVar' receivedMessages (msg :)
          , onConnectivity = const $ pure ()
          }

  msg <- runIO $ generate @(Message SimpleTx) arbitrary

  it "pass the authenticated messages around" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newLabelledTVarIO "received-msgs" []

          withAuthentication
            @(Message SimpleTx)
            @(Message SimpleTx)
            nullTracer
            aliceSk
            [bob]
            ( \NetworkCallback{deliver} _ -> do
                deliver (Signed msg (sign bobSk msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob]

  it "drop message coming from unknown party" $ do
    unexpectedMessage <- ReqTx <$> generate arbitrary
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newLabelledTVarIO "received-msgs" []

          withAuthentication
            @(Message SimpleTx)
            @(Message SimpleTx)
            nullTracer
            aliceSk
            [bob]
            ( \NetworkCallback{deliver} _ -> do
                deliver (Signed msg (sign aliceSk msg) alice)
                deliver (Signed msg (sign bobSk msg) bob)
                deliver (Signed unexpectedMessage (sign carolSk unexpectedMessage) carol)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob, Authenticated msg alice]

  it "drop message coming from party with wrong signature" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newLabelledTVarIO "received-msgs" []

          withAuthentication
            @(Message SimpleTx)
            @(Message SimpleTx)
            nullTracer
            aliceSk
            [bob, carol]
            ( \NetworkCallback{deliver} _ -> do
                deliver (Signed msg (sign carolSk msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` []

  it "authenticate the message to broadcast" $ do
    let someMessage = msg
        sentMsgs = runSimOrThrow $ do
          sentMessages <- newLabelledTVarIO "received-msgs" []

          withAuthentication
            @(Message SimpleTx)
            @(Message SimpleTx)
            nullTracer
            bobSk
            []
            (captureOutgoing sentMessages)
            noopCallback
            $ \Network{broadcast} -> do
              threadDelay 0.6
              broadcast someMessage
              threadDelay 1

          readTVarIO sentMessages

    sentMsgs `shouldBe` [Signed msg (sign bobSk msg) bob]

  it "logs dropped messages" $ do
    let signature = sign carolSk msg
    let signedMsg = Signed msg signature bob
    let traced = runSimOrThrow $ do
          traces <- newLabelledTVarIO "traces" []

          let tracer = traceInTVar traces "AuthenticateSpec"
          withAuthentication
            @(Message SimpleTx)
            @(Message SimpleTx)
            tracer
            aliceSk
            [bob, carol]
            (\NetworkCallback{deliver} _ -> deliver signedMsg)
            noopCallback
            $ \_ ->
              threadDelay 1

          readTVarIO traces

    (message <$> traced) `shouldContain` [mkAuthLog msg signature bob]

  describe "Serialization" $ do
    prop "can roundtrip CBOR encoding/decoding of Signed Hydra Message" $
      prop_canRoundtripCBOREncoding @(Signed Msg)

    roundtripAndGoldenSpecs (Proxy @AuthLog)

newtype Msg = Msg ByteString
  deriving newtype (Eq, Show, ToCBOR, FromCBOR, SignableRepresentation)

instance Arbitrary Msg where
  arbitrary = Msg . pack <$> listOf arbitrary
