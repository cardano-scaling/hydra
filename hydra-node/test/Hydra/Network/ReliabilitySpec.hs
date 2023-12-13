module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head, replicate, unlines)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTQueue, readTVarIO, writeTQueue),
  check,
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  writeTVar,
 )
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (Tracer (..), nullTracer)
import Data.Sequence.Strict ((|>))
import Data.Text qualified as Text
import Data.Vector (Vector, empty, fromList, head, replicate, snoc)
import Data.Vector qualified as Vector
import Hydra.Logging (withTracerOutputTo)
import Hydra.Network (Network (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Reliability (MessagePersistence (..), ReliabilityLog (..), ReliableMsg (..), withReliability)
import Hydra.Node.Network (withFlipHeartbeats)
import Hydra.Persistence (
  Persistence (..),
  PersistenceIncremental (..),
  createPersistence,
  createPersistenceIncremental,
 )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Random (mkStdGen, uniformR)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (
  Positive (Positive),
  arbitraryPrintableChar,
  collect,
  counterexample,
  generate,
  resize,
  tabulate,
  vectorOf,
  within,
  (===),
 )
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, pick, run)
import Prelude (unlines, userError)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _callback action =
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
              , Authenticated (ReliableMsg wellFormedAck (Data "node-3" msg')) carol
              ]

      propagatedMessages `shouldBe` [Authenticated (Data "node-3" msg') carol]

    prop "drops already received messages" $ \(messages :: [Positive Int]) ->
      let
        messagesToSend =
          (\(Positive m) -> Authenticated (ReliableMsg (fromList [0, m, 0]) (Data "node-2" m)) bob)
            <$> messages
        propagatedMessages = aliceReceivesMessages messagesToSend

        receivedMessagesInOrder messageReceived =
          let refMessages = Data "node-2" <$> [1 ..]
              isInMessage Authenticated{payload} = payload `elem` refMessages
           in all isInMessage messageReceived
       in
        receivedMessagesInOrder propagatedMessages
          & counterexample (show propagatedMessages)
          & collect (length propagatedMessages)

  describe "sending messages" $ do
    prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
      let sentMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO empty
            persistence <- mockMessagePersistence 1

            withReliability nullTracer persistence alice [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
              mapM_ (broadcast . Data "node-1") messages

            fromList . Vector.toList <$> readTVarIO sentMessages
       in head . knownMessageIds <$> sentMsgs `shouldBe` fromList [1 .. (length messages)]

    -- this test is quite critical as it demonstrates messages dropped are properly managed and resent to the
    -- other party whatever the length of queue, and whatever the interleaving of threads
    modifyMaxSuccess (const 5000) $
      prop "stress test networking layer" $ \(aliceToBobMessages :: [Int]) (bobToAliceMessages :: [Int]) seed ->
        let
          (msgReceivedByAlice, msgReceivedByBob, traces) = runSimOrThrow $ do
            messagesReceivedByBob <- newTVarIO empty
            messagesReceivedByAlice <- newTVarIO empty
            emittedTraces <- newTVarIO []
            randomSeed <- newTVarIO $ mkStdGen seed
            aliceToBob <- newTQueueIO
            bobToAlice <- newTQueueIO
            alicePersistence <- mockMessagePersistence 2
            bobPersistence <- mockMessagePersistence 2
            let
              -- this is a NetworkComponent that broadcasts authenticated messages
              -- mediated through a read and a write TQueue but drops 0.2 % of them
              aliceFailingNetwork = failingNetwork randomSeed alice (bobToAlice, aliceToBob)
              bobFailingNetwork = failingNetwork randomSeed bob (aliceToBob, bobToAlice)

              bobReliabilityStack = reliabilityStack bobPersistence bobFailingNetwork (captureTraces emittedTraces) "bob" bob [alice]
              aliceReliabilityStack = reliabilityStack alicePersistence aliceFailingNetwork (captureTraces emittedTraces) "alice" alice [bob]

              runAlice = runPeer aliceReliabilityStack "alice" messagesReceivedByAlice messagesReceivedByBob aliceToBobMessages bobToAliceMessages
              runBob = runPeer bobReliabilityStack "bob" messagesReceivedByBob messagesReceivedByAlice bobToAliceMessages aliceToBobMessages

            concurrently_ runAlice runBob

            logs <- readTVarIO emittedTraces
            aliceReceived <- Vector.toList <$> readTVarIO messagesReceivedByAlice
            bobReceived <- Vector.toList <$> readTVarIO messagesReceivedByBob
            pure (aliceReceived, bobReceived, logs)
         in
          within 1000000 $
            msgReceivedByBob
              === aliceToBobMessages
              & counterexample (unlines $ show <$> reverse traces)
              & tabulate "Messages from Alice to Bob" ["< " <> show ((length msgReceivedByBob `div` 10 + 1) * 10)]
              & tabulate "Messages from Bob to Alice" ["< " <> show ((length msgReceivedByAlice `div` 10 + 1) * 10)]

    it "broadcast updates counter from peers" $ do
      let receivedMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO empty
            alicePersistence <- mockMessagePersistence 2
            withReliability
              nullTracer
              alicePersistence
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
            Vector.toList <$> readTVarIO sentMessages

      receivedMsgs `shouldBe` [ReliableMsg (fromList [1, 1]) (Data "node-1" msg)]

    it "appends messages to disk and can load them back" $ do
      withTempDir "network-messages-persistence" $ \tmpDir -> do
        Persistence{load, save} <- createPersistence $ tmpDir <> "/acks"
        PersistenceIncremental{loadAll, append} <- createPersistenceIncremental $ tmpDir <> "/network-messages"

        let messagePersistence =
              MessagePersistence
                { loadAcks = do
                    mloaded <- load
                    case mloaded of
                      Nothing -> pure $ replicate (length [alice, bob]) 0
                      Just acks -> pure acks
                , saveAcks = save
                , loadMessages = loadAll
                , appendMessage = append
                }

        receivedMsgs <- do
          sentMessages <- newTVarIO empty
          withReliability
            nullTracer
            messagePersistence
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
          Vector.toList <$> readTVarIO sentMessages

        receivedMsgs `shouldBe` [ReliableMsg (fromList [1, 1]) (Data "node-1" msg)]

        doesFileExist (tmpDir </> "network-messages") `shouldReturn` True
        loadAll `shouldReturn` [Data "node-1" msg]

        doesFileExist (tmpDir </> "acks") `shouldReturn` True
        load `shouldReturn` Just (fromList [1, 1])
 where
  runPeer reliability partyName receivedMessageContainer sentMessageContainer messagesToSend expectedMessages =
    reliability (capturePayload receivedMessageContainer) $ \Network{broadcast} -> do
      forM_ messagesToSend $ \m -> do
        broadcast (Data partyName m)
        threadDelay 0.1

      concurrently_
        (waitForAllMessages expectedMessages receivedMessageContainer)
        (waitForAllMessages messagesToSend sentMessageContainer)

  reliabilityStack persistence underlyingNetwork tracer nodeId party peers =
    withHeartbeat nodeId noop $
      withFlipHeartbeats $
        withReliability tracer persistence party peers underlyingNetwork

  failingNetwork seed peer (readQueue, writeQueue) callback action =
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
                r <- randomNumber seed
                unless (r < 0.02) $ writeTQueue writeQueue (Authenticated m peer)
            }
  randomNumber seed' = do
    genSeed <- readTVar seed'
    let (res, newGenSeed) = uniformR (0 :: Double, 1) genSeed
    writeTVar seed' newGenSeed
    pure res

noop :: Monad m => b -> m ()
noop = const $ pure ()

aliceReceivesMessages :: [Authenticated (ReliableMsg (Heartbeat msg))] -> [Authenticated (Heartbeat msg)]
aliceReceivesMessages messages = runSimOrThrow $ do
  receivedMessages <- newTVarIO empty
  alicePersistence <- mockMessagePersistence 3

  let baseNetwork incoming _ = mapM incoming messages

      aliceReliabilityStack =
        withReliability
          nullTracer
          alicePersistence
          alice
          [bob, carol]
          baseNetwork

  void $ aliceReliabilityStack (captureIncoming receivedMessages) $ \_action ->
    pure [()]

  Vector.toList <$> readTVarIO receivedMessages

captureIncoming :: MonadSTM m => TVar m (Vector p) -> p -> m ()
captureIncoming receivedMessages msg =
  atomically $ modifyTVar' receivedMessages (`snoc` msg)

capturePayload :: MonadSTM m => TVar m (Vector msg) -> Authenticated (Heartbeat msg) -> m ()
capturePayload receivedMessages Authenticated{payload} = case payload of
  Data _ msg ->
    atomically $ modifyTVar' receivedMessages (`snoc` msg)
  _ -> pure ()

waitForAllMessages :: MonadSTM m => [msg] -> TVar m (Vector msg) -> m ()
waitForAllMessages expectedMessages capturedMessages = atomically $ do
  msgs <- readTVar capturedMessages
  check $ length msgs == length expectedMessages

captureTraces ::
  MonadSTM m =>
  TVar m [ReliabilityLog] ->
  Tracer m ReliabilityLog
captureTraces tvar = Tracer $ \msg -> do
  atomically $ modifyTVar' tvar (msg :)

mockMessagePersistence :: Int -> MonadSTM m => m (MessagePersistence m msg)
mockMessagePersistence numberOfParties = do
  acks <- newTVarIO $ replicate numberOfParties 0
  messages <- newTVarIO mempty
  pure $
    MessagePersistence
      { loadAcks = readTVarIO acks
      , saveAcks = atomically . writeTVar acks
      , loadMessages = toList <$> readTVarIO messages
      , appendMessage = \msg -> atomically $ modifyTVar' messages (|> msg)
      }

realPersistenceFor :: (FromJSON msg, ToJSON msg) => String -> FilePath -> IO (MessagePersistence IO msg)
realPersistenceFor actor tmpDir = do
  Persistence{load, save} <- createPersistence $ tmpDir </> actor </> "acks"
  PersistenceIncremental{loadAll, append} <- createPersistenceIncremental $ tmpDir </> actor </> "network-messages"

  pure $
    MessagePersistence
      { loadAcks = do
          mloaded <- load
          case mloaded of
            Nothing -> pure $ replicate (length [alice, bob]) 0
            Just acks -> pure acks
      , saveAcks = save
      , loadMessages = loadAll
      , appendMessage = append
      }

newtype TestMsg = T Text
  deriving newtype (Eq, Show, ToJSON, FromJSON)

instance Arbitrary TestMsg where
  arbitrary =
    T . Text.pack <$> vectorOf 10000 arbitraryPrintableChar
