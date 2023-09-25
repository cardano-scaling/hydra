{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head, unlines)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO, writeTVar)
import Control.Monad.Class.MonadSay (MonadSay (..))
import Control.Monad.IOSim (Failure (..), IOSim, runSimOrThrow, runSimTrace, traceResult)
import Control.Tracer (Tracer (..), nullTracer)
import Data.List (nub)
import Data.Vector (Vector, empty, fromList, head, snoc)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Reliability (ReliabilityLog (..), ReliableMsg (..), withReliability)
import Hydra.Node.Network (withFlipHeartbeats)
import Prelude (unlines)
import Data.Text (unpack)
import System.Random (mkStdGen, uniformR)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (Positive (Positive), collect, counterexample, forAll, generate, property, resize, suchThat, tabulate, (===))
import Test.Util (printTrace, traceDebug)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (`snoc` msg)}

  let msg' = 42 :: Int
  msg <- Data "node-1" <$> runIO (generate @String arbitrary)

  describe "receiving messages" $ do
    it "forward received messages" $ do
      let propagatedMessages =
            aliceReceivesMessages
              [Authenticated (ReliableMsg (fromList [1, 1, 1]) (Data "node-2" msg)) bob]

      propagatedMessages `shouldBe` [Authenticated (Data "node-2" msg) bob]

    it "do not drop messages with same ids from different peers" $ do
      let propagatedMessages =
            aliceReceivesMessages
              [ Authenticated (ReliableMsg (fromList [0, 1, 0]) (Data "node-2" msg')) bob
              , Authenticated (ReliableMsg (fromList [0, 0, 1]) (Data "node-3" msg')) carol
              ]

      propagatedMessages `shouldBe` [Authenticated (Data "node-2" msg') bob, Authenticated (Data "node-3" msg') carol]

    it "Ignores messages with malformed acks" $ do
      let malFormedAck = fromList [1, 0]
          wellFormedAck = fromList [1, 0, 1]
          propagatedMessages =
            aliceReceivesMessages
              [ Authenticated (ReliableMsg malFormedAck (Data "node-2" msg')) bob
              , Authenticated (ReliableMsg wellFormedAck (Data "node-2" msg')) carol
              ]

      propagatedMessages `shouldBe` [Authenticated (Data "node-2" msg') carol]

    prop "drops already received messages" $ \(messages :: [Positive Int]) ->
      -- FIXME this property will not fail if we drop all the messages
      let
        propagatedMessages = aliceReceivesMessages messagesToSend

        messagesToSend = map (\(Positive m) -> Authenticated (ReliableMsg (fromList [0, m, 0]) (Data "node-2" m)) bob) messages
        receivedMessagesInOrder messageReceived =
          and (zipWith (==) (payload <$> messageReceived) (Data "node-2" <$> [1 ..]))
       in
        receivedMessagesInOrder propagatedMessages
          & counterexample (show propagatedMessages)
          & collect (length propagatedMessages)

    it "garbage collects messages received by all peers" $ do
      let receivedTraces = runSimOrThrow $ do
            emittedTraces <- newTVarIO []
            withReliability
              (captureTraces emittedTraces)
              alice
              [bob, carol]
              ( \incoming action -> do
                  incoming (Authenticated (ReliableMsg (fromList [1, 0, 0]) (Data "node-1" msg)) alice)
                  action $ Network{broadcast = \_ -> pure ()}
                  incoming (Authenticated (ReliableMsg (fromList [1, 1, 0]) (Data "node-2" msg)) bob)
                  action $ Network{broadcast = \_ -> pure ()}
                  incoming (Authenticated (ReliableMsg (fromList [1, 1, 1]) (Data "node-3" msg)) carol)
                  action $ Network{broadcast = \_ -> pure ()}
              )
              noop
              $ \Network{broadcast} -> do
                broadcast (Data "node-1" msg)
            readTVarIO emittedTraces

      receivedTraces `shouldContain` [ClearedMessageQueue{messageQueueLength = 1, deletedMessages = 1}]

  describe "sending messages" $ do
    prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
      let sentMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO empty

            withReliability nullTracer alice [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
              mapM_ (broadcast . Data "node-1") messages

            fromList . toList <$> readTVarIO sentMessages
       in head . messageId <$> sentMsgs `shouldBe` fromList [1 .. (length messages)]

    it "broadcasts messages to single connected peer" $ do
      -- FIXME: this test does not prove more than the above property:
      --        `broadcast messages to the network assigning a sequential id`
      --        we should drop this test as it only proves that this test
      --        custom implementation of the network stack with a queue works
      --        Alternative: explain to @pgrange what he is missing here.
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

    prop "stress test networking layer" $ \seed ->
      forAll (resize 10 (arbitrary :: Gen [Int])) $ \messages ->
        let
          (receivedMessages, traces) = runSimOrThrow $ do
            receivedMessages <- newTVarIO empty
            emittedTraces <- newTVarIO []
            randomSeed <- newTVarIO $ mkStdGen seed
            aliceToBob <- newTQueueIO
            bobToAlice <- newTQueueIO
            let
              waitForAllMessages n receivedMessages messages  = do
                if n == 0
                   then pure ()
                   else do
                    msgs <- readTVarIO receivedMessages
                    if length msgs == length messages
                       then pure ()
                       else threadDelay 1 >> waitForAllMessages (n - 1) receivedMessages messages

              randomNumber = do
                genSeed <- readTVar randomSeed
                let (res, newGenSeed) = uniformR (0 :: Double, 1) genSeed
                writeTVar randomSeed newGenSeed
                pure res

              -- this is a NetworkComponent that broadcasts Alice's authenticated messages
              -- to bob mediated through a TQueue but drop 0.2 % of them
              -- messages are then sent or read from aliceToBob and bobToAlice's queues
              aliceFailingNetwork callback action =
                withAsync
                  ( forever $ do
                      newMsg <- atomically $ readTQueue bobToAlice
                      callback newMsg
                  )
                  $ \_ ->
                    action $
                      Network
                        { broadcast = \m -> atomically $ do
                            -- drop 0.2% of messages
                            r <- randomNumber
                            unless (r < 0.002) $ writeTQueue aliceToBob (Authenticated m alice)
                        }

              -- this is Bob's underlying network that simply propagates messages from and to alice
              -- using aliceToBob and bobToAlice's queue
              bobNetwork callback action =
                withAsync
                  ( forever $ do
                      newMsg <- atomically $ readTQueue aliceToBob
                      callback newMsg
                  )
                  $ \_ ->
                    action $ Network{broadcast = \m -> atomically (writeTQueue bobToAlice (Authenticated m bob))}

              bobReliability =
                withHeartbeat "bob" noop $
                  withFlipHeartbeats $
                    withReliability (captureTraces emittedTraces) bob [alice] bobNetwork

            withReliability (captureTraces emittedTraces) alice [bob] aliceFailingNetwork noop $ \Network{broadcast} ->
              bobReliability (captureIncoming receivedMessages) $ \_ -> do
                forM_ messages $ \m -> do
                  broadcast (Data "alice" m)
                  threadDelay 1

                waitForAllMessages 100 receivedMessages messages


            msgs <- toList <$> readTVarIO receivedMessages
            traces <- readTVarIO emittedTraces
            pure (msgs, traces)

         in (payload <$> receivedMessages) === messages
            & counterexample(unlines $ show <$> reverse traces)

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


noop :: Monad m => b -> m ()
noop = const $ pure ()

aliceReceivesMessages :: [Authenticated (ReliableMsg (Heartbeat msg))] -> [Authenticated (Heartbeat msg)]
aliceReceivesMessages messages = runSimOrThrow $ do
  receivedMessages <- newTVarIO empty

  _ <- withReliability
    nullTracer
    alice
    [bob, carol]
    ( \incoming _ -> do
        mapM incoming messages
    )
    (captureIncoming receivedMessages)
    $ \_ ->
      pure [()]

  toList <$> readTVarIO receivedMessages

captureIncoming :: MonadSTM m => TVar m (Vector p) -> p -> m ()
captureIncoming receivedMessages msg =
  atomically $ modifyTVar' receivedMessages (`snoc` msg)

captureTraces ::
  MonadSTM m =>
  TVar m [ReliabilityLog] ->
  Tracer m ReliabilityLog
captureTraces tvar = Tracer $ \msg -> do
  atomically $ modifyTVar' tvar (msg :)
