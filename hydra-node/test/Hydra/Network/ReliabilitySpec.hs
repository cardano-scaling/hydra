{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (Network (..), NetworkComponent)
import Test.QuickCheck (generate)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (msg :)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (msg :)

  msg <- runIO $ generate @String arbitrary
  it "forward received messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO []

          withReliability
            ( \incoming _ -> do
                incoming msg
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

  it "broadcast messages to the network" $ do
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO []

          withReliability (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            broadcast msg

          readTVarIO sentMessages

    sentMsgs `shouldBe` [msg]

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

withReliability ::
  NetworkComponent m msg a ->
  NetworkComponent m msg a
withReliability withRawNetwork callback action = do
  withRawNetwork dummyCallback dummyBroadcast
 where
  dummyCallback msg = callback msg
  dummyBroadcast Network{broadcast} =
    action $ Network{broadcast = \msg -> broadcast msg}

noop :: Monad m => b -> m ()
noop = const $ pure ()
