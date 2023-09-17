{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.Set as Set
import Data.Vector (empty, fromList, head, snoc)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Reliability (Msg (..), withReliability)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (Positive (Positive), collect, counterexample, forAll, generate, suchThat, tabulate)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (`snoc` msg)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (`snoc` msg)

  msg <- runIO $ generate @String arbitrary

  it "forward received messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty

          withReliability
            nullTracer
            alice
            (fromList [alice, bob])
            ( \incoming _ -> do
                incoming (Authenticated (Msg (fromList [1, 1]) msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob]

  prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO empty

          withReliability nullTracer alice (fromList [alice]) (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            mapM_ (\m -> broadcast (Authenticated m alice)) messages

          fromList . toList <$> readTVarIO sentMessages
     in head . messageId . payload <$> sentMsgs `shouldBe` fromList [1 .. (length messages)]

  it "broadcasts messages to single connected peer" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty
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

          withReliability nullTracer alice (fromList [alice, bob]) aliceNetwork (const $ pure ()) $ \Network{broadcast} ->
            withReliability nullTracer bob (fromList [alice, bob]) bobNetwork (captureIncoming receivedMessages) $ \_ -> do
              broadcast (Authenticated msg alice)
              threadDelay 1

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg alice]

  prop "drops already received messages" $ \(messages :: [Positive Int]) ->
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty

          withReliability
            nullTracer
            alice
            (fromList [alice, bob])
            ( \incoming _ -> do
                forM_ messages $ \(Positive m) ->
                  incoming (Authenticated (Msg (fromList [0, m]) m) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages
        receivedMessagesInOrder =
          and (zipWith (==) (payload <$> receivedMsgs) [1 ..])
     in receivedMessagesInOrder
          & counterexample (show receivedMsgs)
          & collect (length receivedMsgs)

  it "do not drop messages with same ids from different peers" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO empty

          withReliability
            nullTracer
            alice
            (fromList [alice, bob, carol])
            ( \incoming _ -> do
                incoming (Authenticated (Msg (fromList [0, 1, 0]) msg) bob)
                incoming (Authenticated (Msg (fromList [0, 0, 1]) msg) carol)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob, Authenticated msg carol]

  prop "retransmits unacknowledged messages given peer index does not change" $ \(Positive lastMessageKnownToBob) ->
    forAll (arbitrary `suchThat` (> lastMessageKnownToBob)) $ \totalNumberOfMessages ->
      let messagesList = show <$> [1 .. totalNumberOfMessages]
          sentMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO empty

            withReliability
              nullTracer
              alice
              (fromList [alice, bob])
              ( \incoming action -> do
                  concurrently_
                    (action $ Network{broadcast = \m -> atomically $ modifyTVar' sentMessages (`snoc` message (payload m))})
                    ( do
                        threadDelay 2
                        incoming (Authenticated (Msg (fromList [lastMessageKnownToBob, 1]) msg) bob)
                        incoming (Authenticated (Msg (fromList [lastMessageKnownToBob, 1]) msg) bob)
                    )
              )
              noop
              $ \Network{broadcast} -> do
                forM_ messagesList $ \m ->
                  broadcast (Authenticated m alice)
                threadDelay 10

            toList <$> readTVarIO sentMessages
       in length sentMsgs
            <= (2 * totalNumberOfMessages - lastMessageKnownToBob + 1)
            && Set.fromList messagesList == Set.fromList sentMsgs
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
            (fromList [alice, bob])
            ( \incoming action -> do
                concurrently_
                  (action $ Network{broadcast = \m -> atomically $ modifyTVar' sentMessages (`snoc` payload m)})
                  (incoming (Authenticated (Msg (fromList [0, 1]) msg) bob))
            )
            noop
            $ \Network{broadcast} -> do
              threadDelay 1
              broadcast (Authenticated msg bob)
          toList <$> readTVarIO sentMessages

    receivedMsgs `shouldBe` [Msg (fromList [1, 1]) msg]

noop :: Monad m => b -> m ()
noop = const $ pure ()
