{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.API.ServerSpec where

import Hydra.Prelude hiding (decodeUtf8, seq)
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
import Control.Lens ((^?))
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens (key, nonNull)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Hydra.API.Server (RunServerException (..), Server (Server, sendOutput), withAPIServer)
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..), input)
import Hydra.Chain (HeadId (HeadId), PostChainTx (CloseTx), PostTxError (NoSeedInput), confirmedSnapshot)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (PortNumber)
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (utxo), confirmed)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)
import System.IO.Error (isAlreadyInUseError)
import System.Timeout (timeout)
import Test.Hydra.Fixture (alice)
import Test.Network.Ports (withFreePort)
import Test.QuickCheck (checkCoverage, cover, generate)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)

spec :: Spec
spec = describe "ServerSpec" $
  parallel $ do
    it "should fail on port in use" $ do
      showLogsOnFailure $ \tracer -> failAfter 5 $ do
        let withServerOnPort p = withAPIServer @SimpleTx "127.0.0.1" p alice mockPersistence tracer noop
        withFreePort $ \port -> do
          -- We should not be able to start the server on the same port twice
          withServerOnPort port $ \_ ->
            withServerOnPort port (\_ -> failure "should have not started")
              `shouldThrow` \case
                RunServerException{port = errorPort, ioException} ->
                  errorPort == port && isAlreadyInUseError ioException

    it "greets" $ do
      failAfter 5 $
        showLogsOnFailure $ \tracer ->
          withFreePort $ \port ->
            withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \_ -> do
              withClient port "/" $ \conn -> do
                waitMatch 5 conn $ guard . matchGreetings

    it "sends sendOutput to all connected clients" $ do
      queue <- atomically newTQueue
      showLogsOnFailure $ \tracer -> failAfter 5 $
        withFreePort $ \port -> do
          withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
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
      showLogsOnFailure $ \tracer -> failAfter 5 $
        withTempDir "ServerSpec" $ \tmpDir -> do
          let persistentFile = tmpDir <> "/history"
          arbitraryMsg <- generate arbitrary

          persistence <- createPersistenceIncremental persistentFile
          withFreePort $ \port -> do
            withAPIServer @SimpleTx "127.0.0.1" port alice persistence tracer noop $ \Server{sendOutput} -> do
              sendOutput arbitraryMsg

          queue1 <- atomically newTQueue
          queue2 <- atomically newTQueue
          persistence' <- createPersistenceIncremental persistentFile
          withFreePort $ \port -> do
            withAPIServer @SimpleTx "127.0.0.1" port alice persistence' tracer noop $ \Server{sendOutput} -> do
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
          showLogsOnFailure $ \tracer ->
            withFreePort $ \port ->
              withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
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
          showLogsOnFailure $ \tracer ->
            withFreePort $ \port ->
              withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
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
      showLogsOnFailure $ \tracer ->
        withFreePort $ \port ->
          withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
            tx :: SimpleTx <- generate arbitrary
            generatedSnapshot :: Snapshot SimpleTx <- generate arbitrary

            -- The three server output message types which contain transactions
            let txValidMessage = TxValid{headId = HeadId "some-head-id", transaction = tx}
            let sn = generatedSnapshot{confirmed = [tx]}
            let snapShotConfirmedMessage =
                  SnapshotConfirmed
                    { headId = HeadId "some-head-id"
                    , snapshot = sn
                    , signatures = mempty
                    }
            let postTxFailedMessage =
                  PostTxOnChainFailed
                    { postChainTx =
                        CloseTx
                          { confirmedSnapshot =
                              ConfirmedSnapshot
                                { Hydra.Snapshot.snapshot = sn
                                , Hydra.Snapshot.signatures = mempty
                                }
                          }
                    , postTxError = NoSeedInput
                    }
                guardForValue v expected =
                  case v of
                    Aeson.Object km ->
                      guard . (expected ==) =<< KeyMap.lookup "transaction" km
                    _other -> Nothing

            -- client is able to specify they want tx output to be encoded as CBOR
            withClient port "/?history=no&tx-output=cbor" $ \conn -> do
              sendOutput txValidMessage

              waitMatch 5 conn $ \v ->
                guardForValue v (Aeson.String . decodeUtf8 . Base16.encode $ serialize' tx)

              sendOutput snapShotConfirmedMessage

              waitMatch 5 conn $ \v ->
                let expected =
                      Aeson.Array $ fromList [Aeson.String . decodeUtf8 . Base16.encode $ serialize' tx]
                    result =
                      Aeson.encode v ^? key "snapshot" . key "confirmedTransactions" . nonNull
                 in guard $ result == Just expected

              sendOutput postTxFailedMessage

              waitMatch 5 conn $ \v ->
                let expected =
                      Aeson.Array $ fromList [Aeson.String . decodeUtf8 . Base16.encode $ serialize' tx]
                    result =
                      Aeson.encode v ^? key "postChainTx" . key "confirmedSnapshot" . key "snapshot" . key "confirmedTransactions" . nonNull
                 in guard $ result == Just expected

            -- spawn another client but this one wants to see txs in json format
            withClient port "/?history=no" $ \conn -> do
              sendOutput txValidMessage

              waitMatch 5 conn $ \v ->
                guardForValue v (toJSON tx)

    it "removes UTXO from snapshot when clients request it" $
      showLogsOnFailure $ \tracer -> failAfter 5 $
        withFreePort $ \port ->
          withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
            snapshot <- generate arbitrary
            let snapshotConfirmedMessage = SnapshotConfirmed{headId = HeadId "some-head-id", Hydra.API.ServerOutput.snapshot, Hydra.API.ServerOutput.signatures = mempty}

            withClient port "/?snapshot-utxo=no" $ \conn -> do
              sendOutput snapshotConfirmedMessage

              waitMatch 5 conn $ \v ->
                case v of
                  Aeson.Object km -> do
                    case KeyMap.lookup "snapshot" km of
                      Just (Aeson.Object km') -> guard $ isNothing $ KeyMap.lookup "utxo" km'
                      _other -> Nothing
                  _other -> Nothing

    it "sequence numbers are continuous and strictly monotonically increasing" $
      monadicIO $ do
        outputs :: [ServerOutput SimpleTx] <- pick arbitrary
        run $
          showLogsOnFailure $ \tracer -> failAfter 5 $
            withFreePort $ \port ->
              withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
                mapM_ sendOutput outputs
                withClient port "/" $ \conn -> do
                  received <- replicateM (length outputs + 1) (receiveData conn)

                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right (timedOutputs :: [TimedServerOutput SimpleTx]) ->
                      seq <$> timedOutputs `shouldSatisfy` strictlyMonotonic

    it "displays correctly headStatus in a Greeting message" $
      showLogsOnFailure $ \tracer ->
        withFreePort $ \port -> do
          withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
            withClient port "/?history=no" $ \conn -> do
              status <- waitMatch 5 conn $ \v -> v ^? key "headStatus"
              status `shouldBe` Aeson.String "Idle"

            -- TODO: do test commit outputs as well? (or just unit/property test the projection for details)

            headIsOpen <- HeadIsOpen @SimpleTx <$> generate arbitrary <*> generate arbitrary

            sendOutput headIsOpen
            withClient port "/?history=no" $ \conn -> do
              status <- waitMatch 5 conn $ \v -> v ^? key "headStatus"
              status `shouldBe` Aeson.String "Open"

            sendOutput . ReadyToFanout $ headId headIsOpen
            withClient port "/?history=no" $ \conn -> do
              status <- waitMatch 5 conn $ \v -> v ^? key "headStatus"
              status `shouldBe` Aeson.String "ClosedAfterDeadline"

    it "greets with correct head status after restart" $
      showLogsOnFailure $ \tracer ->
        withTempDir "api-server-head-status" $ \persistenceDir ->
          withFreePort $ \port -> do
            apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
            withAPIServer @SimpleTx "127.0.0.1" port alice apiPersistence tracer noop $ \Server{sendOutput} -> do
              headIsInitializing <- generate $ HeadIsInitializing <$> arbitrary <*> arbitrary
              sendOutput headIsInitializing
              withClient port "/?history=no" $ \conn -> do
                status <- waitMatch 5 conn $ \v -> v ^? key "headStatus"
                status `shouldBe` Aeson.String "Initializing"

            -- expect the api server to load events from apiPersistence and project headStatus correctly
            withAPIServer @SimpleTx "127.0.0.1" port alice apiPersistence tracer noop $ \_ -> do
              withClient port "/?history=no" $ \conn -> do
                status <- waitMatch 5 conn $ \v -> v ^? key "headStatus"
                status `shouldBe` Aeson.String "Initializing"

    it "displays correctly snapshotUtxo in a Greeting message" $
      showLogsOnFailure $ \tracer ->
        withFreePort $ \port -> do
          withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \Server{sendOutput} -> do
            generatedSnapshot :: Snapshot SimpleTx <- generate arbitrary

            let snapShotConfirmedMessage =
                  SnapshotConfirmed
                    { headId = HeadId "some-head-id"
                    , snapshot = generatedSnapshot
                    , signatures = mempty
                    }
            let expectedUtxos =
                  toJSON (Hydra.Snapshot.utxo $ Hydra.API.ServerOutput.snapshot snapShotConfirmedMessage)
            sendOutput snapShotConfirmedMessage
            withClient port "/?history=no" $ \conn -> do
              status <- waitMatch 5 conn $ \v -> v ^? key "snapshotUtxo"
              status `shouldBe` expectedUtxos

            -- send another output related to changing the utxo set
            headIsOpen <- HeadIsOpen @SimpleTx <$> generate arbitrary <*> generate arbitrary
            let expectedUtxos' = toJSON $ Hydra.API.ServerOutput.utxo headIsOpen
            sendOutput headIsOpen
            withClient port "/?history=no" $ \conn -> do
              status <- waitMatch 5 conn $ \v -> v ^? key "snapshotUtxo"
              status `shouldBe` expectedUtxos'

    it "greets with correct snapshot utxo after a restart" $
      showLogsOnFailure $ \tracer ->
        withTempDir "api-server-snapshot-utxo" $ \persistenceDir ->
          withFreePort $ \port -> do
            apiPersistence <- createPersistenceIncremental $ persistenceDir <> "/server-output"
            generatedSnapshot :: Snapshot SimpleTx <- generate arbitrary
            let snapShotConfirmedMessage =
                  SnapshotConfirmed
                    { headId = HeadId "some-head-id"
                    , snapshot = generatedSnapshot
                    , signatures = mempty
                    }
            let expectedUtxos =
                  toJSON (Hydra.Snapshot.utxo $ Hydra.API.ServerOutput.snapshot snapShotConfirmedMessage)

            withAPIServer @SimpleTx "127.0.0.1" port alice apiPersistence tracer noop $ \Server{sendOutput} -> do
              sendOutput snapShotConfirmedMessage

              withClient port "/?history=no" $ \conn -> do
                status <- waitMatch 5 conn $ \v -> v ^? key "snapshotUtxo"
                status `shouldBe` expectedUtxos

            -- expect the api server to load events from apiPersistence and project headStatus correctly
            withAPIServer @SimpleTx "127.0.0.1" port alice apiPersistence tracer noop $ \_ -> do
              withClient port "/?history=no" $ \conn -> do
                status <- waitMatch 5 conn $ \v -> v ^? key "snapshotUtxo"
                status `shouldBe` expectedUtxos

    it "sends an error when input cannot be decoded" $
      failAfter 5 $
        withFreePort $
          \port -> sendsAnErrorWhenInputCannotBeDecoded port

strictlyMonotonic :: [Natural] -> Bool
strictlyMonotonic = \case
  [] -> True
  [_] -> True
  (a : b : as) -> a + 1 == b && strictlyMonotonic (b : as)

sendsAnErrorWhenInputCannotBeDecoded :: PortNumber -> Expectation
sendsAnErrorWhenInputCannotBeDecoded port = do
  showLogsOnFailure $ \tracer ->
    withAPIServer @SimpleTx "127.0.0.1" port alice mockPersistence tracer noop $ \_server -> do
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

noop :: Applicative m => a -> m ()
noop = const $ pure ()

withClient :: PortNumber -> String -> (Connection -> IO ()) -> IO ()
withClient port path action = do
  runClient "127.0.0.1" (fromIntegral port) path action

-- | Mocked persistence handle which just does nothing.
mockPersistence :: Applicative m => PersistenceIncremental a m
mockPersistence =
  PersistenceIncremental
    { append = \_ -> pure ()
    , loadAll = pure []
    }

-- | Wait up to some time for an API server output to match the given predicate.
waitMatch :: HasCallStack => Natural -> Connection -> (Aeson.Value -> Maybe a) -> IO a
waitMatch delay con match = do
  seenMsgs <- newTVarIO []
  timeout (fromIntegral delay * 1_000_000) (go seenMsgs) >>= \case
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
shouldSatisfyAll values predicates =
  go values predicates
 where
  go [] [] = pure ()
  go [] _ = failure "shouldSatisfyAll: ran out of values"
  go _ [] = failure "shouldSatisfyAll: ran out of predicates"
  go (v : vs) (p : ps) = do
    v `shouldSatisfy` p
    go vs ps
