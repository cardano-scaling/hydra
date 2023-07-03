module Hydra.Network.AuthenticateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTVarIO), modifyTVar', newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Crypto (HydraKey, Key (SigningKey), Signature, sign, verify)
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.HeartbeatSpec (noop)
import Hydra.Party (Party (..))
import Test.Hydra.Fixture (aliceSk, bob, bobSk)
import Test.QuickCheck (generate)

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

  fit "drop unauthenticated messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [ByteString])

          withAuthentication
            aliceSk
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

data Authenticated msg = Authenticated
  { payload :: msg
  , signature :: Signature msg
  }
  deriving (Eq, Show)

withAuthentication ::
  ( MonadAsync m
  , SignableRepresentation msg
  ) =>
  SigningKey HydraKey ->
  [Party] ->
  NetworkComponent m (Authenticated msg) a ->
  NetworkComponent m msg a
withAuthentication signingKey parties withRawNetwork callback action = do
  withRawNetwork checkSignature authenticate
 where
  checkSignature (Authenticated msg sig) = do
    when (any (\Party{vkey} -> verify vkey sig msg) parties) $
      callback msg
  authenticate = \Network{broadcast} ->
    action $ Network{broadcast = \msg -> broadcast (Authenticated msg (sign signingKey msg))}
