{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.API.ServerSpec where

import Hydra.Prelude hiding (decodeUtf8, seq)
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
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
import Data.Aeson.Lens (key, nonNull)
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog)
import Hydra.API.Server (RunServerException (..), Server (Server, sendOutput), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..), genTimedServerOutput, input)
import Hydra.Chain (
  Chain (Chain),
  PostChainTx (..),
  PostTxError (NoSeedInput),
  confirmedSnapshot,
  draftCommitTx,
  postTx,
  submitTx,
 )
import Hydra.Chain.Direct.Fixture (defaultPParams)
import Hydra.Ledger (txId)
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Network (PortNumber)
import Hydra.Options qualified as Options
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (Snapshot, headId, utxo), confirmed)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)
import System.IO.Error (isAlreadyInUseError)
import Test.Hydra.Fixture (alice, testHeadId)
import Test.Network.Ports (withFreePort)
import Test.QuickCheck (checkCoverage, cover, generate)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)

spec :: Spec
spec = describe "ServerSpec" $
  parallel $ do
    it "should fail on port in use" $ do
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $ do
        let withServerOnPort p = withTestAPIServer p alice mockPersistence tracer
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
            withTestAPIServer port alice mockPersistence tracer $ \_ -> do
              withClient port "/" $ \conn -> do
                waitMatch 5 conn $ guard . matchGreetings

    it "Greetings should contain the hydra-node version" $ do
      failAfter 5 $
        showLogsOnFailure "ServerSpec" $ \tracer ->
          withFreePort $ \port ->
            withTestAPIServer port alice mockPersistence tracer $ \_ -> do
              withClient port "/" $ \conn -> do
                version <- waitMatch 5 conn $ \v -> do
                  guard $ matchGreetings v
                  v ^? key "hydraNodeVersion"
                version `shouldBe` toJSON (showVersion Options.hydraNodeVersion)

    it "sends sendOutput to all connected clients" $ do
      queue <- atomically newTQueue
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
        withFreePort $ \port -> do
          withTestAPIServer port alice mockPersistence tracer $ \Server{sendOutput} -> do
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
                sendOutput arbitraryMsg
                failAfter 1 $ atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [arbitraryMsg, arbitraryMsg]
                failAfter 1 $ atomically (tryReadTQueue queue) `shouldReturn` Nothing

    it "sends all sendOutput history to all connected clients after a restart" $ do
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
        withTempDir "ServerSpec" $ \tmpDir -> do
          let persistentFile = tmpDir <> "/history"
          arbitraryMsg <- generate arbitrary

          persistence <- createPersistenceIncremental persistentFile
          withFreePort $ \port -> do
            withTestAPIServer port alice persistence tracer $ \Server{sendOutput} -> do
              sendOutput arbitraryMsg

          queue1 <- atomically newTQueue
          queue2 <- atomically newTQueue
          persistence' <- createPersistenceIncremental persistentFile
          withFreePort $ \port -> do
            withTestAPIServer port alice persistence' tracer $ \Server{sendOutput} -> do
              semaphore <- newTVarIO 0
              withAsync
                ( concurrently_
                    (withClient port "/" $ testClient queue1 semaphore)
                    (withClient port "/" $ testClient queue2 semaphore)
                )
                $ \_ -> do
                  waitForClients semaphore
                  failAfter 1 $
                    atomically (replicateM 2 (readTQueue queue1))
                      >>= flip shouldSatisfyAll [(==) arbitraryMsg, isGreetings]
                  failAfter 1 $
                    atomically (replicateM 2 (readTQueue queue2))
                      >>= flip shouldSatisfyAll [(==) arbitraryMsg, isGreetings]

                  sendOutput arbitraryMsg
                  failAfter 1 $
                    atomically (replicateM 1 (readTQueue queue1))
                      `shouldReturn` [arbitraryMsg]
                  failAfter 1 $
                    atomically (replicateM 1 (readTQueue queue2))
                      `shouldReturn` [arbitraryMsg]
                  failAfter 1 $
                    atomically (tryReadTQueue queue1)
                      `shouldReturn` Nothing

    it "echoes history (past outputs) to client upon reconnection" $
      checkCoverage . monadicIO $ do
        outputs <- pick arbitrary
        monitor $ cover 0.1 (null outputs) "no message when reconnecting"
        monitor $ cover 0.1 (length outputs == 1) "only one message when reconnecting"
        monitor $ cover 1 (length outputs > 1) "more than one message when reconnecting"
        run $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreePort $ \port ->
              withTestAPIServer port alice mockPersistence tracer $ \Server{sendOutput} -> do
                mapM_ sendOutput outputs
                withClient port "/" $ \conn -> do
                  received <- failAfter 5 $ replicateM (length outputs + 1) (receiveData conn)
                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right timedOutputs -> do
                      let actualOutputs = output <$> timedOutputs
                      List.init actualOutputs `shouldBe` outputs
                      List.last actualOutputs `shouldSatisfy` isGreetings

    it "does not echo history if client says no" $
      checkCoverage . monadicIO $ do
        history :: [ServerOutput SimpleTx] <- pick arbitrary
        monitor $ cover 0.1 (null history) "no message when reconnecting"
        monitor $ cover 0.1 (length history == 1) "only one message when reconnecting"
        monitor $ cover 1 (length history > 1) "more than one message when reconnecting"
        run $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreePort $ \port ->
              withTestAPIServer port alice mockPersistence tracer $ \Server{sendOutput} -> do
                let sendFromApiServer = sendOutput
                mapM_ sendFromApiServer history
                -- start client that doesn't want to see the history
                withClient port "/?history=no" $ \conn -> do
                  -- wait on the greeting message
                  waitMatch 5 conn $ guard . matchGreetings

                  notHistoryMessage :: ServerOutput SimpleTx <- generate arbitrary
                  sendFromApiServer notHistoryMessage

                  -- Receive one more message. The messages we sent
                  -- before client connected are ignored as expected and client can
                  -- see only this last sent message.
                  received <- replicateM 1 (receiveData conn)

                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right timedOutputs' -> do
                      (output <$> timedOutputs') `shouldBe` [notHistoryMessage]

    it "outputs tx as cbor or json depending on the client" $
      showLogsOnFailure "ServerSpec" $ \tracer ->
        withFreePort $ \port ->
          withTestAPIServer port alice mockPersistence tracer $ \Server{sendOutput} -> do
            tx :: SimpleTx <- generate arbitrary
            generatedSnapshot :: Snapshot SimpleTx <- generate arbitrary

            let Snapshot{headId} = generatedSnapshot
            -- The three server output message types which contain transactions
            let txValidMessage = TxValid{headId = headId, transaction = tx}
            let sn = generatedSnapshot{confirmed = [txId tx]}
            let snapShotConfirmedMessage =
                  SnapshotConfirmed
                    { headId = headId
                    , snapshot = sn
                    , signatures = mempty
                    }

            let postTxFailedMessage =
                  PostTxOnChainFailed
                    { postChainTx =
                        CloseTx
                          { headId
                          , confirmedSnapshot =
                              ConfirmedSnapshot
                                { Hydra.Snapshot.snapshot = sn
                                , Hydra.Snapshot.signatures = mempty
                                }
                          }
                    , postTxError = NoSeedInput
                    }
                guardForValue v expected =
                  guard $ v ^? key "transaction" == Just expected

            -- client is able to specify they want tx output to be encoded as CBOR
            withClient port "/?history=no&tx-output=cbor" $ \conn -> do
              sendOutput txValidMessage

              waitMatch 5 conn $ \v ->
                guardForValue v (Aeson.String . decodeUtf8 . Base16.encode $ serialize' tx)

              sendOutput snapShotConfirmedMessage

              let expectedTxIds = Aeson.Array $ fromList [toJSON (txId tx)]

              waitMatch 5 conn $ \v ->
                let result =
                      Aeson.encode v ^? key "snapshot" . key "confirmedTransactions" . nonNull
                 in guard $ result == Just expectedTxIds

              sendOutput postTxFailedMessage

              waitMatch 5 conn $ \v ->
                let result =
                      Aeson.encode v ^? key "postChainTx" . key "confirmedSnapshot" . key "snapshot" . key "confirmedTransactions" . nonNull
                 in guard $ result == Just expectedTxIds

            -- spawn another client but this one wants to see txs in json format
            withClient port "/?history=no" $ \conn -> do
              sendOutput txValidMessage

              waitMatch 5 conn $ \v ->
                guardForValue v (toJSON tx)

    it "removes UTXO from snapshot when clients request it" $
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
        withFreePort $ \port ->
          withTestAPIServer port alice mockPersistence tracer $ \Server{sendOutput} -> do
            snapshot <- generate arbitrary
            let snapshotConfirmedMessage =
                  SnapshotConfirmed
                    { headId = testHeadId
                    , Hydra.API.ServerOutput.snapshot
                    , Hydra.API.ServerOutput.signatures = mempty
                    }

            withClient port "/?snapshot-utxo=no" $ \conn -> do
              sendOutput snapshotConfirmedMessage

              waitMatch 5 conn $ \v ->
                guard $ isNothing $ v ^? key "utxo"

    it "sequence numbers are continuous and strictly monotonically increasing" $
      monadicIO $ do
        outputs :: [ServerOutput SimpleTx] <- pick arbitrary
        run $
          showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
            withFreePort $ \port ->
              withTestAPIServer port alice mockPersistence tracer $ \Server{sendOutput} -> do
                mapM_ sendOutput outputs
                withClient port "/" $ \conn -> do
                  received <- replicateM (length outputs + 1) (receiveData conn)

                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right (timedOutputs :: [TimedServerOutput SimpleTx]) ->
                      seq <$> timedOutputs `shouldSatisfy` strictlyMonotonic

    it "displays correctly headStatus and snapshotUtxo in a Greeting message" $
      showLogsOnFailure "ServerSpec" $ \tracer ->
        withFreePort $ \port -> do
          -- Prime some relevant server outputs already into persistence to
          -- check whether the latest headStatus is loaded correctly.
          existingServerOutputs <-
            generate $
              mapM
                (>>= genTimedServerOutput)
                [ HeadIsInitializing <$> arbitrary <*> arbitrary
                , HeadIsAborted <$> arbitrary <*> arbitrary
                , HeadIsFinalized <$> arbitrary <*> arbitrary
                ]
          let persistence = mockPersistence' existingServerOutputs

          withTestAPIServer port alice persistence tracer $ \Server{sendOutput} -> do
            let generateSnapshot =
                  generate $
                    SnapshotConfirmed <$> arbitrary <*> arbitrary <*> arbitrary

            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" . key "tag" == Just (Aeson.String "Final")
              -- test that the 'snapshotUtxo' is excluded from json if there is no utxo
              guard $ isNothing (v ^? key "snapshotUtxo")

            (headId, headIsOpenMsg) <- generate $ do
              headId <- arbitrary
              output <- HeadIsOpen headId <$> arbitrary
              pure (headId, output)
            snapShotConfirmedMsg@SnapshotConfirmed{snapshot = Snapshot{utxo}} <-
              generateSnapshot

            mapM_ sendOutput [headIsOpenMsg, snapShotConfirmedMsg]
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" . key "tag" == Just (Aeson.String "Open")
              guard $ v ^? key "snapshotUtxo" == Just (toJSON utxo)

            snapShotConfirmedMsg'@SnapshotConfirmed{snapshot = Snapshot{utxo = utxo'}} <-
              generateSnapshot
            let readyToFanoutMsg = ReadyToFanout{headId}

            mapM_ sendOutput [readyToFanoutMsg, snapShotConfirmedMsg']
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" . key "tag" == Just (Aeson.String "FanoutPossible")
              guard $ v ^? key "snapshotUtxo" == Just (toJSON utxo')

    it "greets with correct head status and snapshot utxo after restart" $
      showLogsOnFailure "ServerSpec" $ \tracer ->
        withTempDir "api-server-head-status" $ \persistenceDir ->
          withFreePort $ \port -> do
            let generateSnapshot =
                  generate $
                    SnapshotConfirmed <$> arbitrary <*> arbitrary <*> arbitrary
            apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
            snapShotConfirmedMsg@SnapshotConfirmed{snapshot = Snapshot{utxo}} <-
              generateSnapshot
            let expectedUtxos = toJSON utxo

            withTestAPIServer port alice apiPersistence tracer $ \Server{sendOutput} -> do
              headIsInitializing <- generate $ HeadIsInitializing <$> arbitrary <*> arbitrary

              mapM_ sendOutput [headIsInitializing, snapShotConfirmedMsg]
              waitForValue port $ \v -> do
                guard $ v ^? key "headStatus" . key "tag" == Just (Aeson.String "Initializing")
                guard $ v ^? key "snapshotUtxo" == Just expectedUtxos

            -- expect the api server to load events from apiPersistence and project headStatus correctly
            withTestAPIServer port alice apiPersistence tracer $ \_ -> do
              waitForValue port $ \v -> do
                guard $ v ^? key "headStatus" . key "tag" == Just (Aeson.String "Initializing")
                guard $ v ^? key "snapshotUtxo" == Just expectedUtxos

    it "sends an error when input cannot be decoded" $
      failAfter 5 $
        withFreePort $
          \port -> sendsAnErrorWhenInputCannotBeDecoded port

strictlyMonotonic :: (Eq a, Enum a) => [a] -> Bool
strictlyMonotonic = \case
  [] -> True
  [_] -> True
  (a : b : as) -> succ a == b && strictlyMonotonic (b : as)

sendsAnErrorWhenInputCannotBeDecoded :: PortNumber -> Expectation
sendsAnErrorWhenInputCannotBeDecoded port = do
  showLogsOnFailure "ServerSpec" $ \tracer ->
    withTestAPIServer port alice mockPersistence tracer $ \_server -> do
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
    Right TimedServerOutput{output = resp} -> do
      atomically (writeTQueue queue resp)
      testClient queue semaphore cnx

dummyChainHandle :: Chain tx IO
dummyChainHandle =
  Chain
    { postTx = \_ -> error "unexpected call to postTx"
    , draftCommitTx = \_ -> error "unexpected call to draftCommitTx"
    , submitTx = \_ -> error "unexpected call to submitTx"
    }

noop :: Applicative m => a -> m ()
noop = const $ pure ()

withTestAPIServer ::
  PortNumber ->
  Party ->
  PersistenceIncremental (TimedServerOutput SimpleTx) IO ->
  Tracer IO APIServerLog ->
  (Server SimpleTx IO -> IO ()) ->
  IO ()
withTestAPIServer port actor persistence tracer =
  withAPIServer @SimpleTx "127.0.0.1" port actor persistence tracer dummyChainHandle defaultPParams noop

withClient :: PortNumber -> String -> (Connection -> IO ()) -> IO ()
withClient port path action = do
  runClient "127.0.0.1" (fromIntegral port) path action

-- | Mocked persistence handle which just does nothing.
mockPersistence :: Applicative m => PersistenceIncremental a m
mockPersistence =
  mockPersistence' []

-- | Mocked persistence which does not contain some constant elements.
mockPersistence' :: Applicative m => [a] -> PersistenceIncremental a m
mockPersistence' xs =
  PersistenceIncremental
    { append = \_ -> pure ()
    , loadAll = pure xs
    }

waitForValue :: HasCallStack => PortNumber -> (Aeson.Value -> Maybe ()) -> IO ()
waitForValue port f =
  withClient port "/?history=no" $ \conn ->
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
