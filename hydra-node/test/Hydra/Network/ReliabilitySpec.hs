{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO, writeTVar)
import Control.Concurrent.Class.MonadSTM.TVar (modifyTVar)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Sequence ((|>))
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Party (Party)
import Test.Hydra.Fixture (alice, bob)
import Test.QuickCheck (generate)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (|> msg)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

  msg <- runIO $ generate @String arbitrary

  it "forward received messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO []

          withReliability alice
            ( \incoming _ -> do
                incoming (Authenticated (Msg 1 msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

  prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO mempty

          withReliability alice (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            mapM_ broadcast messages

          toList <$> readTVarIO sentMessages
     in messageId . payload <$> sentMsgs `shouldBe` [1 .. (length messages)]

  it "broadcasts messages to single connected peer" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO []
          queue <- newTQueueIO

          let aliceNetwork _ action = do
                action (Network{broadcast = atomically . writeTQueue queue})

          let bobNetwork callback action = do
                withAsync
                  ( forever $ do
                      newMsg <- atomically $ readTQueue queue
                      callback newMsg
                  )
                  $ \_ ->
                    action (Network{broadcast = const $ pure ()})

          withReliability alice aliceNetwork (const $ pure ()) $ \Network{broadcast} ->
            withReliability bob bobNetwork (captureIncoming receivedMessages) $ \_ -> do
              broadcast msg
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

  it "drops already received messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO []

          withReliability
            alice
            ( \incoming _ -> do
                incoming (Authenticated (Msg 1 msg) bob)
                incoming (Authenticated (Msg 1 msg) bob)
                incoming (Authenticated (Msg 4 msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

data Msg msg = Msg
  { messageId :: Int
  , message :: msg
  }
  deriving (Eq, Show)

withReliability ::
  (MonadSTM m) =>
  Party ->
  NetworkComponent m (Authenticated (Msg msg)) a ->
  NetworkComponent m msg a
withReliability us withRawNetwork callback action = do
  broadcastCounter <- newTVarIO 0
  incomingCounter <- newTVarIO 0
  withRawNetwork (dummyCallback incomingCounter) (dummyBroadcast broadcastCounter)
 where
  dummyBroadcast messageCounter Network{broadcast} =
    action $
      Network
        { broadcast = \msg -> do
            counter <- atomically $ do
              modifyTVar messageCounter (+ 1)
              readTVar messageCounter

            broadcast $ Authenticated (Msg counter msg) us
        }

  dummyCallback messageCounter (Authenticated (Msg n msg) party) = do
    count <- readTVarIO messageCounter
    when (n == count + 1) $ do
      atomically $ writeTVar messageCounter n
      callback msg

noop :: Monad m => b -> m ()
noop = const $ pure ()
