module Hydra.Network.AuthenticateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Hydra.Crypto (Signature, sign, verify)
import Hydra.Network (Network (..), NetworkComponent, NodeId (..))
import Hydra.Network.HeartbeatSpec (noop)
import Hydra.Party (Party(..))
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

  fit "pass the authenticated messages around" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [ByteString])

          withAuthentication
            [bob]
            ( \incoming _ -> do
                incoming (Authenticated "1" (sign bobSk "1"))
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` ["1"]

  fit "drop unauthenticated messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [ByteString])

          withAuthentication
            [bob]
            ( \incoming _ -> do
                incoming (Authenticated "1" (sign bobSk "1"))
                incoming (Authenticated "2" (sign aliceSk "1"))
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` ["1"]

  fit "authenticate the message to broadcast" $ do
    let someMessage = "1"
        sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO ([] :: [Authenticated ByteString])

          withAuthentication [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            threadDelay 0.6
            broadcast someMessage
            threadDelay 1

          readTVarIO sentMessages

    sentMsgs `shouldBe` [Authenticated "1" (sign bobSk "1")]

data Authenticated msg = Authenticated
  { payload :: msg
  , signature :: Signature msg
  }
  deriving (Eq, Show)

withAuthentication ::
  ( MonadAsync m
  , MonadDelay m
  , MonadMonotonicTime m
  , SignableRepresentation msg
  ) =>
  [Party] ->
  NetworkComponent m (Authenticated msg) a ->
  NetworkComponent m msg a
withAuthentication parties withRawNetwork callback action = do
  withRawNetwork checkSignature authenticate
 where
  checkSignature (Authenticated msg sig) = do
    when (any (\Party{vkey} -> verify vkey sig msg) parties) $
      callback msg
  authenticate = \Network{broadcast} ->
    action $ Network{broadcast = \msg -> broadcast (Authenticated msg (sign bobSk msg))}
