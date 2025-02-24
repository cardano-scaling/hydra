{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.API.ServerSpec where

import Hydra.Prelude hiding (decodeUtf8, seq)
import Test.Hydra.Prelude

import Conduit (yieldMany)
import Control.Concurrent.Class.MonadSTM (
  check,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
 )
import Control.Lens ((^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key)
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (hPutStrLn)
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog)
import Hydra.API.Server (APIServerConfig (..), RunServerException (..), Server (Server, sendOutput), mapStateChangedToServerOutput, withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..), input)
import Hydra.API.ServerOutputFilter (ServerOutputFilter (..))
import Hydra.Chain (
  Chain (Chain),
  draftCommitTx,
  draftDepositTx,
  postTx,
  submitTx,
 )
import Hydra.Events (EventSource (..), StateEvent (..), genStateEvent)
import Hydra.HeadLogic.Outcome (genStateChanged)
import Hydra.HeadLogic.Outcome qualified as Outcome
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Network (PortNumber)
import Hydra.Options qualified as Options
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot (Snapshot, utxo))
import Network.Simple.WSS qualified as WSS
import Network.TLS (ClientHooks (onServerCertificate), ClientParams (clientHooks), defaultParamsClient)
import Network.WebSockets (Connection, ConnectionException, receiveData, runClient, sendBinaryData)
import System.IO.Error (isAlreadyInUseError)
import Test.Hydra.Tx.Fixture (alice, defaultPParams, testEnvironment, testHeadId)
import Test.Hydra.Tx.Gen ()
import Test.Network.Ports (withFreePort)
import Test.QuickCheck (checkCoverage, cover, generate)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)
import Test.Util (isContinuous)

spec :: Spec
spec =
  parallel $ do
    it "should fail on port in use" $ do
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $ do
        let withServerOnPort p = withTestAPIServer p alice (mockSource []) tracer
        withFreePort $ \port -> do
          -- We should not be able to start the server on the same port twice
          withServerOnPort port $ \_ ->
            withServerOnPort port (\_ -> failure "should have not started")
              `shouldThrow` \case
                RunServerException{port = errorPort, ioException} ->
                  errorPort == port && isAlreadyInUseError ioException

    it "greets" $ do
      failAfter 5 $
        showLogsOnFailure "ServerSpec" $ \tracer ->
          withFreePort $ \port ->
            withTestAPIServer port alice (mockSource []) tracer $ \_ -> do
              withClient port "/" $ \conn -> do
                waitMatch 5 conn $ guard . matchGreetings

    it "Greetings should contain the hydra-node version" $ do
      failAfter 5 $
        showLogsOnFailure "ServerSpec" $ \tracer ->
          withFreePort $ \port ->
            withTestAPIServer port alice (mockSource []) tracer $ \_ -> do
              withClient port "/" $ \conn -> do
                version <- waitMatch 5 conn $ \v -> do
                  guard $ matchGreetings v
                  v ^? key "hydraNodeVersion"
                version `shouldBe` toJSON (showVersion Options.hydraNodeVersion)

    it "sends sendOutput to all connected clients" $ do
      queue <- atomically newTQueue
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
        withFreePort $ \port -> do
          withTestAPIServer port alice (mockSource []) tracer $ \Server{sendOutput} -> do
            semaphore <- newTVarIO 0
            withAsync
              ( concurrently_
                  (withClient port "/" $ testClient queue semaphore)
                  (withClient port "/" $ testClient queue semaphore)
              )
              $ \_ -> do
                waitForClients semaphore
                failAfter 1 $
                  atomically (replicateM 2 (readTQueue queue))
                    >>= (`shouldSatisfyAll` [isGreetings, isGreetings])

                arbitraryMsg <- generate arbitrary
                let arbitraryServerOutput = fromMaybe (error "failed to convert in mapStateChangedToServerOutput") (mapStateChangedToServerOutput arbitraryMsg)
                arbitraryStateEvent <- generate $ genStateEvent arbitraryMsg
                sendOutput arbitraryStateEvent
                failAfter 1 $ atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [arbitraryServerOutput, arbitraryServerOutput]
                failAfter 1 $ atomically (tryReadTQueue queue) `shouldReturn` Nothing

    it "sends all sendOutput history to all connected clients after a restart" $ do
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $ do
        arbitraryMsg <- generate arbitrary
        let arbitraryServerOutput = fromMaybe (error "failed to convert in mapStateChangedToServerOutput") (mapStateChangedToServerOutput arbitraryMsg)
        stateEvent <- generate $ genStateEvent arbitraryMsg
        let eventSource = mockSource [stateEvent]

        withFreePort $ \port -> do
          withTestAPIServer port alice eventSource tracer $ \_ ->
            pure ()

        queue1 <- atomically newTQueue
        queue2 <- atomically newTQueue
        withFreePort $ \port -> do
          withTestAPIServer port alice eventSource tracer $ \Server{sendOutput} -> do
            semaphore <- newTVarIO 0
            withAsync
              ( concurrently_
                  (withClient port "/?history=yes" $ testClient queue1 semaphore)
                  (withClient port "/?history=yes" $ testClient queue2 semaphore)
              )
              $ \_ -> do
                waitForClients semaphore
                failAfter 1 $
                  atomically (replicateM 2 (readTQueue queue1))
                    >>= flip shouldSatisfyAll [(==) arbitraryServerOutput, isGreetings]
                failAfter 1 $
                  atomically (replicateM 2 (readTQueue queue2))
                    >>= flip shouldSatisfyAll [(==) arbitraryServerOutput, isGreetings]
                sendOutput stateEvent
                failAfter 1 $
                  atomically (replicateM 1 (readTQueue queue1))
                    `shouldReturn` [arbitraryServerOutput]
                failAfter 1 $
                  atomically (replicateM 1 (readTQueue queue2))
                    `shouldReturn` [arbitraryServerOutput]
                failAfter 1 $
                  atomically (tryReadTQueue queue1)
                    `shouldReturn` Nothing

    it "echoes history (past outputs) to client upon reconnection" $
      checkCoverage . monadicIO $ do
        outputs <- pick $ mapM (genStateEvent <=< genStateChanged) =<< arbitrary
        monitor $ cover 0.1 (null outputs) "no message when reconnecting"
        monitor $ cover 0.1 (length outputs == 1) "only one message when reconnecting"
        monitor $ cover 1 (length outputs > 1) "more than one message when reconnecting"
        run $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreePort $ \port ->
              withTestAPIServer port alice (mockSource outputs) tracer $ \Server{sendOutput} -> do
                mapM_ sendOutput outputs
                withClient port "/?history=yes" $ \conn -> do
                  received <- failAfter 20 $ replicateM (length outputs + 1) (receiveData conn)
                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right timedOutputs -> do
                      let actualOutputs = output <$> timedOutputs
                      List.init actualOutputs `shouldBe` List.init (mapMaybe (mapStateChangedToServerOutput . stateChanged) outputs)
                      List.last actualOutputs `shouldSatisfy` isGreetings

    it "does not echo history if client says no" $
      checkCoverage . monadicIO $ do
        history :: [StateEvent SimpleTx] <- pick arbitrary
        monitor $ cover 0.1 (null history) "no message when reconnecting"
        monitor $ cover 0.1 (length history == 1) "only one message when reconnecting"
        monitor $ cover 1 (length history > 1) "more than one message when reconnecting"
        run $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreePort $ \port ->
              withTestAPIServer port alice (mockSource history) tracer $ \Server{sendOutput} -> do
                let sendFromApiServer = sendOutput
                mapM_ sendFromApiServer history
                -- start client that doesn't want to see the history
                withClient port "/?history=yes" $ \conn -> do
                  -- wait on the greeting message
                  waitMatch 5 conn $ guard . matchGreetings

                  notHistoryMessage :: StateEvent SimpleTx <- generate arbitrary
                  sendFromApiServer notHistoryMessage

                  -- Receive one more message. The messages we sent
                  -- before client connected are ignored as expected and client can
                  -- see only this last sent message.
                  received <- replicateM 1 (receiveData conn)

                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right timedOutputs' -> do
                      (output <$> timedOutputs') `shouldBe` [fromMaybe (error "failed to convert in mapStateChangedToServerOutput") (mapStateChangedToServerOutput $ stateChanged notHistoryMessage)]

    it "removes UTXO from snapshot when clients request it" $
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
        withFreePort $ \port ->
          withTestAPIServer port alice (mockSource []) tracer $ \Server{sendOutput} -> do
            snapshot <- generate arbitrary
            snapshotConfirmedMessage <-
              generate $
                genStateEvent $
                  Outcome.SnapshotConfirmed
                    { headId = testHeadId
                    , snapshot
                    , signatures = mempty
                    }

            withClient port "/?snapshot-utxo=no" $ \conn -> do
              sendOutput snapshotConfirmedMessage

              waitMatch 5 conn $ \v ->
                guard $ isNothing $ v ^? key "utxo"

    it "sequence numbers are continuous" $
      monadicIO $ do
        outputs' <- pick $ mapM (genStateEvent <=< genStateChanged) =<< arbitrary
        -- XXX: here we manually update eventId's to be sequential since it is easier than in the arbitrary instance.
        -- API Server should keep these intact and just produce corresponding sequence numbers. In real life persistence
        -- would handle this behavior anyway.
        let outputs = zipWith (\stateEvent eventId -> stateEvent{eventId}) outputs' [1 ..]
        run $
          showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
            withFreePort $ \port ->
              withTestAPIServer port alice (mockSource outputs) tracer $ \Server{sendOutput} -> do
                mapM_ sendOutput outputs
                withClient port "/?history=yes" $ \conn -> do
                  received <- replicateM (length outputs + 1) (receiveData conn)

                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right (timedOutputs :: [TimedServerOutput SimpleTx]) ->
                      seq <$> timedOutputs `shouldSatisfy` isContinuous

    it "displays correctly headStatus and snapshotUtxo in a Greeting message" $
      showLogsOnFailure "ServerSpec" $ \tracer ->
        withFreePort $ \port -> do
          -- Prime some relevant server outputs already into persistence to
          -- check whether the latest headStatus is loaded correctly.
          existingStateChanges <-
            generate $
              mapM
                (>>= genStateEvent)
                [ Outcome.HeadInitialized <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                , Outcome.HeadAborted <$> arbitrary <*> arbitrary <*> arbitrary
                , Outcome.HeadFannedOut <$> arbitrary <*> arbitrary <*> arbitrary
                ]
          let eventSource = mockSource existingStateChanges

          withTestAPIServer port alice eventSource tracer $ \Server{sendOutput} -> do
            let generateSnapshot =
                  Outcome.SnapshotConfirmed <$> arbitrary <*> arbitrary <*> arbitrary

            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Final")
              -- test that the 'snapshotUtxo' is excluded from json if there is no utxo
              guard $ isNothing (v ^? key "snapshotUtxo")

            (headId, headIsOpenMsg) <- generate $ do
              headId <- arbitrary
              output <- genStateEvent =<< (Outcome.HeadOpened headId <$> arbitrary <*> arbitrary)
              pure (headId, output)
            snapShotConfirmedMsg@StateEvent{stateChanged = Outcome.SnapshotConfirmed{snapshot = Snapshot{utxo}}} <-
              generate $ genStateEvent =<< generateSnapshot

            mapM_ sendOutput [headIsOpenMsg, snapShotConfirmedMsg]
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Open")
              guard $ v ^? key "snapshotUtxo" == Just (toJSON utxo)

            snapShotConfirmedMsg'@StateEvent{stateChanged = Outcome.SnapshotConfirmed{snapshot = Snapshot{utxo = utxo'}}} <-
              generate $ genStateEvent =<< generateSnapshot
            readyToFanoutMsg <- generate $ genStateEvent Outcome.HeadIsReadyToFanout{headId}

            mapM_ sendOutput [readyToFanoutMsg, snapShotConfirmedMsg']
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "FanoutPossible")
              guard $ v ^? key "snapshotUtxo" == Just (toJSON utxo')

    it "greets with correct head status and snapshot utxo after restart" $
      showLogsOnFailure "ServerSpec" $ \tracer ->
        withFreePort $ \port -> do
          let generateSnapshot =
                generate $
                  Outcome.SnapshotConfirmed <$> arbitrary <*> arbitrary <*> arbitrary
          snapShotConfirmedMsg@Outcome.SnapshotConfirmed{snapshot = Snapshot{utxo}} <-
            generateSnapshot
          headIsInitializing :: Outcome.StateChanged SimpleTx <- generate $ Outcome.HeadInitialized <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          let expectedUtxos = toJSON utxo
          stateEvents :: [StateEvent SimpleTx] <- generate $ mapM genStateEvent [snapShotConfirmedMsg, headIsInitializing]
          let eventSource = mockSource stateEvents

          withTestAPIServer port alice eventSource tracer $ \_ -> do
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Initializing")
              guard $ v ^? key "snapshotUtxo" == Just expectedUtxos

          withTestAPIServer port alice eventSource tracer $ \_ -> do
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Initializing")
              guard $ v ^? key "snapshotUtxo" == Just expectedUtxos

    it "sends an error when input cannot be decoded" $
      failAfter 5 $
        withFreePort $
          \port -> sendsAnErrorWhenInputCannotBeDecoded port

    describe "TLS support" $ do
      it "accepts TLS connections when configured" $ do
        showLogsOnFailure "ServerSpec" $ \tracer ->
          withFreePort $ \port -> do
            let config =
                  APIServerConfig
                    { host = "127.0.0.1"
                    , port
                    , tlsCertPath = Just "test/tls/certificate.pem"
                    , tlsKeyPath = Just "test/tls/key.pem"
                    }
            withAPIServer @SimpleTx config testEnvironment alice (mockSource []) tracer dummyChainHandle defaultPParams allowEverythingServerOutputFilter noop $ \_ -> do
              let clientParams = defaultParamsClient "127.0.0.1" ""
                  allowAnyParams =
                    clientParams{clientHooks = (clientHooks clientParams){onServerCertificate = \_ _ _ _ -> pure []}}
              WSS.connect allowAnyParams "127.0.0.1" (show port) "/" [] $ \(conn, _) -> do
                waitMatch 5 conn $ guard . matchGreetings

sendsAnErrorWhenInputCannotBeDecoded :: PortNumber -> Expectation
sendsAnErrorWhenInputCannotBeDecoded port = do
  showLogsOnFailure "ServerSpec" $ \tracer ->
    withTestAPIServer port alice (mockSource []) tracer $ \_server -> do
      withClient port "/" $ \con -> do
        _greeting :: ByteString <- receiveData con
        sendBinaryData con invalidInput
        msg <- receiveData con
        case Aeson.eitherDecode @(TimedServerOutput SimpleTx) msg of
          Left{} -> failure $ "Failed to decode output " <> show msg
          Right TimedServerOutput{output = resp} -> resp `shouldSatisfy` isInvalidInput
 where
  invalidInput = "not a valid message"
  isInvalidInput = \case
    InvalidInput{input} -> input == invalidInput
    _ -> False

matchGreetings :: Aeson.Value -> Bool
matchGreetings v =
  v ^? key "tag" == Just (Aeson.String "Greetings")

isGreetings :: ServerOutput tx -> Bool
isGreetings = \case
  Greetings{} -> True
  _ -> False

waitForClients :: (MonadSTM m, Ord a, Num a) => TVar m a -> m ()
waitForClients semaphore = atomically $ readTVar semaphore >>= \n -> check (n >= 2)

-- NOTE: this client runs indefinitely so it should be run within a context that won't
-- leak runaway threads
testClient :: TQueue IO (ServerOutput SimpleTx) -> TVar IO Int -> Connection -> IO ()
testClient queue semaphore cnx = do
  atomically $ modifyTVar' semaphore (+ 1)
  msg <- receiveData cnx
  case Aeson.eitherDecode msg of
    Left{} -> failure $ "Failed to decode message " <> show msg
    Right StateEvent{stateChanged} -> do
      mapStateChangedToServerOutput stateChanged & \case
        Nothing -> pure ()
        Just resp -> do
          atomically (writeTQueue queue resp)
          testClient queue semaphore cnx

dummyChainHandle :: Chain tx IO
dummyChainHandle =
  Chain
    { postTx = \_ -> error "unexpected call to postTx"
    , draftCommitTx = \_ -> error "unexpected call to draftCommitTx"
    , draftDepositTx = \_ -> error "unexpected call to draftDepositTx"
    , submitTx = \_ -> error "unexpected call to submitTx"
    }

allowEverythingServerOutputFilter :: ServerOutputFilter tx
allowEverythingServerOutputFilter =
  ServerOutputFilter
    { txContainsAddr = \_ _ -> True
    }

noop :: Applicative m => a -> m ()
noop = const $ pure ()

withTestAPIServer ::
  PortNumber ->
  Party ->
  EventSource (StateEvent SimpleTx) IO ->
  Tracer IO APIServerLog ->
  (Server SimpleTx IO -> IO ()) ->
  IO ()
withTestAPIServer port actor eventSource tracer action = do
  withAPIServer @SimpleTx config testEnvironment actor eventSource tracer dummyChainHandle defaultPParams allowEverythingServerOutputFilter noop action
 where
  config = APIServerConfig{host = "127.0.0.1", port, tlsCertPath = Nothing, tlsKeyPath = Nothing}

-- | Connect to a websocket server running at given path. Fails if not connected
-- within 2 seconds.
withClient :: PortNumber -> String -> (Connection -> IO ()) -> IO ()
withClient port path action =
  connect (20 :: Int)
 where
  connect !n
    | n < 0 = failure "withClient could not connect"
    | otherwise =
        runClient "127.0.0.1" (fromIntegral port) path action
          `catch` \(e :: ConnectionException) -> do
            hPutStrLn stderr $ "withClient failed to connect: " <> show e
            threadDelay 0.1
            connect (n - 1)

mockSource :: Monad m => [a] -> EventSource a m
mockSource events =
  EventSource
    { sourceEvents = yieldMany events
    }

waitForValue :: HasCallStack => PortNumber -> (Aeson.Value -> Maybe ()) -> IO ()
waitForValue port f =
  withClient port "/?history=yes" $ \conn ->
    waitMatch 5 conn f

-- | Wait up to some time for an API server output to match the given predicate.
waitMatch :: HasCallStack => Natural -> Connection -> (Aeson.Value -> Maybe a) -> IO a
waitMatch delay con match = do
  seenMsgs <- newTVarIO []
  timeout (fromIntegral delay) (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "waitMatch did not match a message within " <> show delay <> "s"
            , padRight ' ' 20 "  seen messages:"
                <> unlines (align 20 (decodeUtf8 . toStrict . Aeson.encode <$> msgs))
            ]
 where
  go seenMsgs = do
    msg <- waitNext con
    atomically (modifyTVar' seenMsgs (msg :))
    maybe (go seenMsgs) pure (match msg)

  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q

  waitNext connection = do
    bytes <- receiveData connection
    case Aeson.eitherDecode' bytes of
      Left err -> failure $ "WaitNext failed to decode msg: " <> err
      Right value -> pure value

shouldSatisfyAll :: Show a => [a] -> [a -> Bool] -> Expectation
shouldSatisfyAll = go
 where
  go [] [] = pure ()
  go [] _ = failure "shouldSatisfyAll: ran out of values"
  go _ [] = failure "shouldSatisfyAll: ran out of predicates"
  go (v : vs) (p : ps) = do
    v `shouldSatisfy` p
    go vs ps
