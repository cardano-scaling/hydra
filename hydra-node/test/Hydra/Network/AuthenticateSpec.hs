module Hydra.Network.AuthenticateSpec where

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO, writeTVar), modifyTVar')
import Control.Monad.IOSim (runSimOrThrow)
import Data.ByteString (pack)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (Envelope (message), nullTracer, traceInTVar)
import Hydra.Network (Network (..), NetworkCallback (..))
import Hydra.Network.Authenticate (AuthLog, Authenticated (..), Signed (Signed), mkAuthLog, withAuthentication)
import Hydra.Network.Message (Message (ReqTx))
import Hydra.NetworkSpec (prop_canRoundtripCBOREncoding)
import Hydra.Prelude
import Hydra.Tx.Crypto (sign)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Network.Authenticate ()
import Test.Hydra.Prelude
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.QuickCheck (listOf)
import Test.QuickCheck.Gen (generate)
import Test.Util (noopCallback)

spec :: Spec
spec = parallel $ do
  let captureOutgoing :: MonadSTM m => TVar m [a] -> p -> (Network m a -> b) -> b
      captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :), memberAdd = \_ -> pure ()}

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
            (pure [bob])
            (pure Nothing)
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
            (pure [bob])
            (pure Nothing)
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
            (pure [bob, carol])
            (pure Nothing)
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
            (pure [])
            (pure Nothing)
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
            (pure [bob, carol])
            (pure Nothing)
            (\NetworkCallback{deliver} _ -> deliver signedMsg)
            noopCallback
            $ \_ ->
              threadDelay 1

          readTVarIO traces

    (message <$> traced) `shouldContain` [mkAuthLog msg signature bob]

  it "speculatively accepts messages from a joining party while a join is pending" $ do
    -- Phase 2 of dynamic-head-participants (issue #1813): the joining party's
    -- own 'AckSn' must be accepted before the on-chain UpdateParametersTx is
    -- observed (because that's the message that lets the snapshot finalize).
    -- The auth layer reads 'readJoiningParty'; while it returns 'Just p',
    -- messages from 'p' are accepted even though 'p' is not in the live
    -- party set. Once the joining-party accessor returns 'Nothing', messages
    -- from 'p' should be dropped again.
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newLabelledTVarIO "received-msgs" []
          joining <- newLabelledTVarIO "joining-party" (Just carol)

          withAuthentication
            @(Message SimpleTx)
            @(Message SimpleTx)
            nullTracer
            aliceSk
            (pure [bob])
            (readTVar joining)
            ( \NetworkCallback{deliver} _ -> do
                -- carol is the joining party — accepted.
                deliver (Signed msg (sign carolSk msg) carol)
                -- The join is complete; clear the speculative-accept.
                atomically $ writeTVar joining Nothing
                -- carol is now an outsider — rejected.
                deliver (Signed msg (sign carolSk msg) carol)
                -- bob's message is always accepted.
                deliver (Signed msg (sign bobSk msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    -- Order is reversed because captureIncoming prepends.
    receivedMsgs `shouldBe` [Authenticated msg bob, Authenticated msg carol]

  it "drops messages from a party that was just removed from the live set" $ do
    -- dynamic-head-participants (issue #1813): the accepted-parties set is
    -- an 'STM' action; after we shrink it via 'writeTVar', subsequent
    -- inbound messages from the removed party must be rejected.
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newLabelledTVarIO "received-msgs" []
          accepted <- newLabelledTVarIO "accepted-parties" [bob, carol]

          withAuthentication
            @(Message SimpleTx)
            @(Message SimpleTx)
            nullTracer
            aliceSk
            (readTVar accepted)
            (pure Nothing)
            ( \NetworkCallback{deliver} _ -> do
                deliver (Signed msg (sign bobSk msg) bob)
                -- Carol leaves the head: she is removed from the accepted set.
                atomically $ writeTVar accepted [bob]
                -- A subsequent message from Carol must be dropped.
                deliver (Signed msg (sign carolSk msg) carol)
                -- Bob remains accepted.
                deliver (Signed msg (sign bobSk msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob, Authenticated msg bob]

  describe "Serialization" $ do
    prop "can roundtrip CBOR encoding/decoding of Signed Hydra Message" $
      prop_canRoundtripCBOREncoding @(Signed Msg)

    roundtripAndGoldenSpecs (Proxy @AuthLog)

newtype Msg = Msg ByteString
  deriving newtype (Eq, Show, ToCBOR, FromCBOR, SignableRepresentation)

instance Arbitrary Msg where
  arbitrary = Msg . pack <$> listOf arbitrary
