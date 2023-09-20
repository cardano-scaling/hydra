{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.List (nub)
import Data.Vector (empty, fromList, head, snoc)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..))
import Hydra.Network.Reliability (ReliabilityException (ReliabilityReceivedAckedMalformed), ReliableMsg (..), withReliability)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (Positive (Positive), collect, counterexample, forAll, generate, suchThat, tabulate)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (`snoc` msg)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (`snoc` msg)

  let msg' = 42 :: Int
  msg <- Data "node-1" <$> runIO (generate @String arbitrary)

  it "forward received messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty

          withReliability
            nullTracer
            alice
            [bob]
            ( \incoming _ -> do
                incoming (Authenticated (ReliableMsg (fromList [1, 1]) (Data "node-2" msg)) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated (Data "node-2" msg) bob]

  prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO empty

          withReliability nullTracer alice [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            mapM_ (broadcast . Data "node-1") messages

          fromList . toList <$> readTVarIO sentMessages
     in head . messageId <$> sentMsgs `shouldBe` fromList [1 .. (length messages)]

  it "broadcasts messages to single connected peer" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty
          queue <- newTQueueIO

          let aliceNetwork _ action = do
                action (Network{broadcast = atomically . writeTQueue queue . flip Authenticated alice})

          let bobNetwork callback action = do
                withAsync
                  ( forever $ do
                      newMsg <- atomically $ readTQueue queue
                      callback newMsg
                  )
                  $ \_ ->
                    action (Network{broadcast = const $ pure ()})

          withReliability nullTracer alice [bob] aliceNetwork (const $ pure ()) $ \Network{broadcast} ->
            withReliability nullTracer bob [alice] bobNetwork (captureIncoming receivedMessages) $ \_ -> do
              broadcast (Data "node-1" msg)
              threadDelay 1

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated (Data "node-1" msg) alice]

  prop "drops already received messages" $ \(messages :: [Positive Int]) ->
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty

          withReliability
            nullTracer
            alice
            [bob]
            ( \incoming _ -> do
                forM_ messages $ \(Positive m) ->
                  incoming (Authenticated (ReliableMsg (fromList [0, m]) (Data "node-2" m)) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages
        receivedMessagesInOrder =
          and (zipWith (==) (payload <$> receivedMsgs) (Data "node-2" <$> [1 ..]))
     in receivedMessagesInOrder
          & counterexample (show receivedMsgs)
          & collect (length receivedMsgs)

  it "do not drop messages with same ids from different peers" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty

          withReliability
            nullTracer
            alice
            [bob, carol]
            ( \incoming _ -> do
                incoming (Authenticated (ReliableMsg (fromList [0, 1, 0]) (Data "node-2" msg')) bob)
                incoming (Authenticated (ReliableMsg (fromList [0, 0, 1]) (Data "node-3" msg')) carol)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated (Data "node-2" msg') bob, Authenticated (Data "node-3" msg') carol]

  prop "retransmits unacknowledged messages given peer index does not change" $ \(Positive lastMessageKnownToBob) ->
    forAll (arbitrary `suchThat` (> lastMessageKnownToBob)) $ \totalNumberOfMessages ->
      let messagesList = Data "node-1" <$> [1 .. totalNumberOfMessages]
          sentMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO empty

            withReliability
              nullTracer
              alice
              [bob]
              ( \incoming action -> do
                  concurrently_
                    (action $ Network{broadcast = \m -> atomically $ modifyTVar' sentMessages (`snoc` message m)})
                    ( do
                        threadDelay 2
                        incoming (Authenticated (ReliableMsg (fromList [lastMessageKnownToBob, 1]) (Data "node-2" msg')) bob)
                        incoming (Authenticated (ReliableMsg (fromList [lastMessageKnownToBob, 1]) (Data "node-2" msg')) bob)
                    )
              )
              noop
              $ \Network{broadcast} -> do
                forM_ messagesList $ \m ->
                  broadcast m
                threadDelay 10

            toList <$> readTVarIO sentMessages
       in length sentMsgs
            <= (2 * totalNumberOfMessages - lastMessageKnownToBob + 1)
            && nub messagesList == nub sentMsgs
            & counterexample ("number of missing messages: " <> show (totalNumberOfMessages - lastMessageKnownToBob))
            & counterexample ("sent messages: " <> show sentMsgs)
            & counterexample ("total messages: " <> show messagesList)
            & tabulate "Resent" [show (length sentMsgs - totalNumberOfMessages)]

  it "broadcast updates counter from peers" $ do
    let receivedMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO empty
          withReliability
            nullTracer
            alice
            [bob]
            ( \incoming action -> do
                concurrently_
                  (action $ Network{broadcast = \m -> atomically $ modifyTVar' sentMessages (`snoc` m)})
                  (incoming (Authenticated (ReliableMsg (fromList [0, 1]) (Data "node-2" msg)) bob))
            )
            noop
            $ \Network{broadcast} -> do
              threadDelay 1
              broadcast (Data "node-1" msg)
          toList <$> readTVarIO sentMessages

    receivedMsgs `shouldBe` [ReliableMsg (fromList [1, 1]) (Data "node-1" msg)]

  it "Fails with ReliabilityReceivedAckedMalformed if length acks /= all parties" $ do
    withReliability
      nullTracer
      alice
      [bob, carol]
      ( \incoming _ -> do
          incoming (Authenticated (ReliableMsg (fromList [1, 0]) (Data "node-2" msg')) bob)
      )
      noop
      noop
      `shouldThrow` \case
        ReliabilityReceivedAckedMalformed -> True
        _ -> False

noop :: Monad m => b -> m ()
noop = const $ pure ()
