{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head, unlines)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTQueue, readTVarIO, writeTQueue),
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  writeTVar,
 )
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (Tracer (..), nullTracer)
import Data.Vector (Vector, empty, fromList, head, snoc)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Reliability (ReliabilityLog (..), ReliableMsg (..), withReliability)
import Hydra.Node.Network (withFlipHeartbeats)
import System.Random (mkStdGen, uniformR)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (
  Positive (Positive),
  collect,
  counterexample,
  generate,
  tabulate,
  (===),
 )
import Prelude (unlines)

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
       in head . knownMessageIds <$> sentMsgs `shouldBe` fromList [1 .. (length messages)]

    prop "stress test networking layer" $ \(aliceToBobMessages :: [Int]) (bobToAliceMessages :: [Int]) seed ->
      let
        (msgReceivedByAlice, msgReceivedByBob, traces) = runSimOrThrow $ do
          messagesReceivedByBob <- newTVarIO empty
          messagesReceivedByAlice <- newTVarIO empty
          emittedTraces <- newTVarIO []
          randomSeed <- newTVarIO $ mkStdGen seed
          aliceToBob <- newTQueueIO
          bobToAlice <- newTQueueIO
          let
            randomNumber = do
              genSeed <- readTVar randomSeed
              let (res, newGenSeed) = uniformR (0 :: Double, 1) genSeed
              writeTVar randomSeed newGenSeed
              pure res

            -- this is a NetworkComponent that broadcasts authenticated messages
            -- mediated through a read and a write TQueue but drops 0.2 % of them
            aliceFailingNetwork = failingNetwork alice (bobToAlice, aliceToBob)
            bobFailingNetwork = failingNetwork bob (aliceToBob, bobToAlice)
            failingNetwork peer (readQueue, writeQueue) callback action =
              withAsync
                ( forever $ do
                    newMsg <- atomically $ readTQueue readQueue
                    callback newMsg
                )
                $ \_ ->
                  action $
                    Network
                      { broadcast = \m -> atomically $ do
                          -- drop 2% of messages
                          r <- randomNumber
                          unless (r < 0.02) $ writeTQueue writeQueue (Authenticated m peer)
                      }

            bobReliabilityStack = reliabilityStack bobFailingNetwork "bob" bob [alice]
            aliceReliabilityStack = reliabilityStack aliceFailingNetwork "alice" alice [bob]
            reliabilityStack underlyingNetwork partyName party peers =
              withHeartbeat partyName noop $
                withFlipHeartbeats $
                  withReliability (captureTraces emittedTraces) party peers underlyingNetwork

            runAlice = runPeer aliceReliabilityStack "alice" messagesReceivedByAlice bobToAliceMessages
            runBob = runPeer bobReliabilityStack "bob" messagesReceivedByBob aliceToBobMessages
            runPeer reliability partyName receivedMessageContainer expectedMessages =
              reliability (capturePayload receivedMessageContainer) $ \Network{broadcast} -> do
                forM_ aliceToBobMessages $ \m -> do
                  broadcast (Data partyName m)
                  threadDelay 1

                waitForAllMessages 100 expectedMessages receivedMessageContainer
                threadDelay 10

          race_ runAlice runBob

          logs <- readTVarIO emittedTraces
          aliceReceived <- toList <$> readTVarIO messagesReceivedByAlice
          bobReceived <- toList <$> readTVarIO messagesReceivedByBob
          pure (aliceReceived, bobReceived, logs)
       in
        msgReceivedByBob
          === aliceToBobMessages
          & counterexample (unlines $ show <$> reverse traces)
          & tabulate "Messages from Alice to Bob" ["< " <> show ((length msgReceivedByBob `div` 10 + 1) * 10)]
          & tabulate "Messages from Bob to Alice" ["< " <> show ((length msgReceivedByAlice `div` 10 + 1) * 10)]

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

  let baseNetwork incoming _ = mapM incoming messages

      aliceReliabilityStack =
        withReliability
          nullTracer
          alice
          [bob, carol]
          baseNetwork

  void $ aliceReliabilityStack (captureIncoming receivedMessages) $ \_action ->
    pure [()]

  toList <$> readTVarIO receivedMessages

captureIncoming :: MonadSTM m => TVar m (Vector p) -> p -> m ()
captureIncoming receivedMessages msg =
  atomically $ modifyTVar' receivedMessages (`snoc` msg)

capturePayload :: (MonadSTM m) => TVar m (Vector msg) -> Authenticated (Heartbeat msg) -> m ()
capturePayload receivedMessages message = case payload message of
  Data _ msg ->
    atomically $ modifyTVar' receivedMessages (`snoc` msg)
  _ -> pure ()

waitForAllMessages :: (MonadSTM m, MonadDelay m) => Int -> [msg] -> TVar m (Vector msg) -> m ()
waitForAllMessages n expectedMessages capturedMessages = do
  if n == (0 :: Int)
    then pure ()
    else do
      msgs <- readTVarIO capturedMessages
      if length msgs == length expectedMessages
        then pure ()
        else threadDelay 1 >> waitForAllMessages (n - 1) expectedMessages capturedMessages

captureTraces ::
  MonadSTM m =>
  TVar m [ReliabilityLog] ->
  Tracer m ReliabilityLog
captureTraces tvar = Tracer $ \msg -> do
  atomically $ modifyTVar' tvar (msg :)
