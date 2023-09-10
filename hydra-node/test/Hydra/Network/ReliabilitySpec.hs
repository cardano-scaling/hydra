{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import qualified Data.List as List
import Data.Sequence ((|>))
import qualified Data.Set as Set
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Reliability (Msg (..), withReliability)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (Positive (Positive), collect, counterexample, forAll, forAllShrink, generate, suchThat, tabulate, (===))

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (|> msg)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (|> msg)

  msg <- runIO $ generate @String arbitrary

  it "forward received messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO mempty

          withReliability
            alice
            [alice, bob]
            ( \incoming _ -> do
                incoming (Authenticated (Msg [1, 1] msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob]

  prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO mempty

          withReliability alice [alice] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            mapM_ (\m -> broadcast (Authenticated m alice)) messages

          toList <$> readTVarIO sentMessages
     in List.head . messageId . payload <$> sentMsgs `shouldBe` [1 .. (length messages)]

  it "broadcasts messages to single connected peer" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO mempty
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

          withReliability alice [alice, bob] aliceNetwork (const $ pure ()) $ \Network{broadcast} ->
            withReliability bob [alice, bob] bobNetwork (captureIncoming receivedMessages) $ \_ -> do
              broadcast (Authenticated msg alice)
              threadDelay 1

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg alice]

  prop "drops already received messages" $ \(messages :: [Positive Int]) ->
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO mempty

          withReliability
            alice
            [alice, bob]
            ( \incoming _ -> do
                forM_ messages $ \(Positive m) ->
                  incoming (Authenticated (Msg [0, m] m) bob)
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
          receivedMessages <- newTVarIO mempty

          withReliability
            alice
            [alice, bob, carol]
            ( \incoming _ -> do
                incoming (Authenticated (Msg [0, 1, 0] msg) bob)
                incoming (Authenticated (Msg [0, 0, 1] msg) carol)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [Authenticated msg bob, Authenticated msg carol]

  prop "sends unacknowledged messages" $ \(Positive lastMessageKnownToBob) ->
    forAll (arbitrary `suchThat` (> lastMessageKnownToBob)) $ \totalNumberOfMessages ->
      let messagesList = show <$> [1 .. totalNumberOfMessages]
          sentMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO mempty

            withReliability
              alice
              [alice, bob]
              ( \incoming action -> do
                  concurrently_
                    (action $ Network{broadcast = \_ -> atomically $ modifyTVar' sentMessages (|> msg)})
                    (threadDelay 2 >> incoming (Authenticated (Msg [lastMessageKnownToBob, 1] msg) bob))
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
            & counterexample ("number of sent messages: " <> show (length sentMsgs))
            & tabulate "Resent" [show (length sentMsgs - totalNumberOfMessages)]

noop :: Monad m => b -> m ()
noop = const $ pure ()
