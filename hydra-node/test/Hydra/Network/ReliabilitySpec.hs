{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO)
import Control.Concurrent.Class.MonadSTM.TVar (modifyTVar)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Sequence ((|>))
import Hydra.Network (Network (..), NetworkComponent)
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

          withReliability
            ( \incoming _ -> do
                incoming (Msg 1 msg)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

  prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO mempty

          withReliability (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            mapM_ broadcast messages

          toList <$> readTVarIO sentMessages
     in messageId <$> sentMsgs `shouldBe` [1 .. (length messages)]

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

          withReliability aliceNetwork (const $ pure ()) $ \Network{broadcast} ->
            withReliability bobNetwork (captureIncoming receivedMessages) $ \_ -> do
              broadcast msg
              threadDelay 1

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

data Msg msg = Msg
  { messageId :: Int
  , message :: msg
  }
  deriving (Eq, Show)

withReliability ::
  (MonadSTM m) =>
  NetworkComponent m (Msg msg) a ->
  NetworkComponent m msg a
withReliability withRawNetwork callback action = do
  counter <- newTVarIO 0
  withRawNetwork dummyCallback (dummyBroadcast counter)
 where
  dummyCallback (Msg _ msg) = callback msg
  dummyBroadcast messageCounter Network{broadcast} =
    action $
      Network
        { broadcast = \msg -> do
            counter <- atomically $ do
              modifyTVar messageCounter (+ 1)
              readTVar messageCounter

            broadcast (Msg counter msg)
        }

noop :: Monad m => b -> m ()
noop = const $ pure ()
