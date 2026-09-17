{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.API.ServerSpec where

import Hydra.Prelude hiding (decodeUtf8, seq)
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Conduit (yieldMany)
import Control.Concurrent.Class.MonadSTM (
  check,
  modifyTVar',
  readTQueue,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
 )
import Control.Lens ((^?))
import Control.Tracer.JSON (Tracer, showLogsOnFailure)
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Number, _String)
import Data.EventSource (EventSink (..), EventSource (..), HasEventId (getEventId))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (hPutStrLn)
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog)
import Hydra.API.ClientInput (ClientInput (Init))
import Hydra.API.Server (APIServerConfig (..), RunServerException (..), Server, mkTimedServerOutputFromStateEvent, projectCommitInfo, projectNetworkInfo, sendMessage, withAPIServer)
import Hydra.API.ServerOutput (ApiEncoding (..), ApiMessage (..), ClientMessage (..), CommitInfo (..), InvalidInput (..), NetworkInfo (..), ServerOutput (..), ServerOutputConfig (..), TimedServerOutput (..), WithAddressedTx (..), WithUTxO (..), input)
import Hydra.API.ServerOutputFilter (ServerOutputFilter (..))
import Hydra.API.WSServer (mkServerOutputConfig, queryParamsOf, shouldServeHistory)
import Hydra.Chain (
  Chain (Chain),
  checkNonADAAssets,
  draftDepositTx,
  postTx,
  submitTx,
 )
import Hydra.HeadLogic.Outcome qualified as Outcome
import Hydra.HeadLogic.State (FanoutMode (..))
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.HeadLogicSpec (inIdleState, inOpenState, testSnapshot)
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Network (Host (..), PortNumber)
import Hydra.NetworkVersions qualified as NetworkVersions
import Hydra.Options (defaultRunOptions)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.IsTx (txId, utxoFromTx)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot (Snapshot, utxo, utxoToCommit))
import Network.Simple.WSS qualified as WSS
import Network.Socket (Socket, close)
import Network.TLS (ClientHooks (onServerCertificate), ClientParams (clientHooks), defaultParamsClient)
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets (Connection, ConnectionException, receiveData, runClient, sendBinaryData)
import System.IO.Error (isAlreadyInUseError)
import Test.Hydra.HeadLogic.StateEvent (genStateEvent)
import Test.Hydra.Ledger.Simple (aValidTx, utxoRefs)
import Test.Hydra.Node.Fixture (testEnvironment)
import Test.Hydra.Tx.Fixture (alice, defaultPParams, testHeadId)
import Test.Hydra.Tx.Gen ()
import Test.Network.Ports (withFreePort)
import Test.QuickCheck (checkCoverage, cover, forAllShrink, generate, listOf, suchThat)
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ConstructorArbitraryPair (..), toADTArbitrary)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)

spec :: Spec
spec =
  do
    it "should fail on port in use" $ do
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $ do
        let withServerOnPort p = withTestAPIServerBindingPort p alice (mockSource []) tracer
        -- Deliberately takes a bare port rather than a bound socket: the point
        -- is that the server binds it itself, so the second attempt is refused.
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
          withFreeServerSocket $ \sock port ->
            withTestAPIServer sock port alice (mockSource []) tracer $ \_ -> do
              withClient port "/" $ \conn -> do
                waitMatch 5 conn $ guard . matchGreetings

    it "Greetings should contain the hydra-node version" $ do
      failAfter 5 $
        showLogsOnFailure "ServerSpec" $ \tracer ->
          withFreeServerSocket $ \sock port ->
            withTestAPIServer sock port alice (mockSource []) tracer $ \_ -> do
              withClient port "/" $ \conn -> do
                version <- waitMatch 5 conn $ \v -> do
                  guard $ matchGreetings v
                  v ^? key "hydraNodeVersion"
                version `shouldBe` toJSON (showVersion NetworkVersions.hydraNodeVersion)

    it "sends server outputs to all connected clients" $ do
      queue <- newLabelledTQueueIO "queue"
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
        withFreeServerSocket $ \sock port -> do
          withTestAPIServer sock port alice (mockSource []) tracer $ \(EventSink{putEvent}, _) -> do
            semaphore <- newLabelledTVarIO "semaphore" 0
            withAsyncLabelled
              ( "concurrent-test-clients"
              , concurrentlyLabelled_
                  ("concurrent-test-client-1", withClient port "/" $ testClient queue semaphore)
                  ("concurrent-test-client-2", withClient port "/" $ testClient queue semaphore)
              )
              $ \_ -> do
                waitForClients semaphore
                failAfter 10 $
                  atomically (replicateM 2 (readTQueue queue))
                    >>= (`shouldSatisfyAll` [matchGreetings, matchGreetings])

                arbitraryEvent <- generate genStateEventForApi
                let expectedMessage =
                      toJSON $
                        fromMaybe (error "failed to convert stateEvent") $
                          mkTimedServerOutputFromStateEvent Nothing arbitraryEvent
                putEvent arbitraryEvent
                failAfter 10 $ atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [expectedMessage, expectedMessage]
                failAfter 10 $ atomically (tryReadTQueue queue) `shouldReturn` Nothing

    it "sends server output history to all connected clients (using given event source)" $ do
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $ do
        stateEvent <- generate genStateEventForApi
        let expectedMessage =
              toJSON $
                fromMaybe (error "failed to convert stateEvent") $
                  mkTimedServerOutputFromStateEvent Nothing stateEvent
        let eventSource = mockSource [stateEvent]

        queue1 <- newLabelledTQueueIO "queue1"
        queue2 <- newLabelledTQueueIO "queue2"
        withFreeServerSocket $ \sock port -> do
          withTestAPIServer sock port alice eventSource tracer $ \_ -> do
            semaphore <- newLabelledTVarIO "semaphore" 0
            withAsyncLabelled
              ( "concurrent-test-clients"
              , concurrentlyLabelled_
                  ("concurrent-test-client-queue1", withClient port "/?history=yes" $ testClient queue1 semaphore)
                  ("concurrent-test-client-queue2", withClient port "/?history=yes" $ testClient queue2 semaphore)
              )
              $ \_ -> do
                waitForClients semaphore
                failAfter 10 $ do
                  atomically (readTQueue queue1) `shouldReturn` expectedMessage
                  atomically (readTQueue queue1) >>= (`shouldSatisfy` matchGreetings)
                failAfter 10 $ do
                  atomically (readTQueue queue2) `shouldReturn` expectedMessage
                  atomically (readTQueue queue2) >>= (`shouldSatisfy` matchGreetings)

    it "echoes history (past outputs) to client upon reconnection" $
      forAllShrink (listOf genStateEventForApi) shrink $ \events -> do
        let expectedMessages = map toJSON $ mapMaybe (mkTimedServerOutputFromStateEvent Nothing) events
        checkCoverage . monadicIO $ do
          monitor $ cover 0.1 (null events) "no message when reconnecting"
          monitor $ cover 0.1 (length events == 1) "only one message when reconnecting"
          monitor $ cover 1 (length events > 1) "more than one message when reconnecting"
          run $
            showLogsOnFailure "ServerSpec" $ \tracer ->
              withFreeServerSocket $ \sock port ->
                withTestAPIServer sock port alice (mockSource events) tracer $ \(EventSink{putEvent}, _) -> do
                  mapM_ putEvent events
                  withClient port "/?history=yes" $ \conn -> do
                    received <- failAfter 20 $ replicateM (length events + 1) (receiveData conn)
                    case traverse Aeson.eitherDecode received of
                      Left{} -> failure $ "Failed to decode messages:\n" <> show received
                      Right actualMessages -> do
                        List.init actualMessages `shouldBe` expectedMessages
                        List.last actualMessages `shouldSatisfy` matchGreetings

    it "does not echo history if client says no" $
      checkCoverage . monadicIO $ do
        history <- pick $ listOf genStateEventForApi
        monitor $ cover 0.1 (null history) "no message when reconnecting"
        monitor $ cover 0.1 (length history == 1) "only one message when reconnecting"
        monitor $ cover 1 (length history > 1) "more than one message when reconnecting"
        run $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreeServerSocket $ \sock port ->
              withTestAPIServer sock port alice (mockSource history) tracer $ \(EventSink{putEvent}, _) -> do
                mapM_ putEvent history
                -- start client that doesn't want to see the history. Passing
                -- 'history=no' and passing nothing at all take the same branch
                -- of 'shouldServeHistory', so this covers the default too.
                withClient port "/?history=no" $ \conn -> do
                  -- NOTE: Assert on the *first* message rather than draining up
                  -- to the greeting. 'wsApp' forwards history before the
                  -- greeting, so a 'waitMatch' for the greeting would swallow
                  -- any replay and pass whether or not history was served.
                  --
                  -- The longer-than-typical budget here is because this is a
                  -- property test running ~100 iterations; each iteration spins
                  -- up a full WS server, and the per-iteration 5s default was
                  -- racing with CPU contention when the full hydra-node test
                  -- suite runs in parallel.
                  greeting <- failAfter 20 $ receiveData conn
                  case Aeson.eitherDecode greeting of
                    Left{} -> failure $ "Failed to decode greeting:\n" <> show greeting
                    Right (v :: Value) -> v `shouldSatisfy` matchGreetings

                  notHistoryMessage :: StateEvent SimpleTx <- generate genStateEventForApi
                  putEvent notHistoryMessage

                  -- Receive one more message. The messages we sent
                  -- before client connected are ignored as expected and client can
                  -- see only this last sent message.
                  received <- failAfter 20 $ replicateM 1 (receiveData conn)

                  case traverse Aeson.eitherDecode received of
                    Left{} -> failure $ "Failed to decode messages:\n" <> show received
                    Right timedOutputs -> do
                      timedOutputs `shouldBe` [fromMaybe (error "failed to convert stateEvent") $ mkTimedServerOutputFromStateEvent Nothing notHistoryMessage]

    it "removes UTXO from snapshot when clients request it" $
      showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 5 $
        withFreeServerSocket $ \sock port ->
          withTestAPIServer sock port alice (mockSource []) tracer $ \(EventSink{putEvent}, _) -> do
            snapshot <- generate arbitrary
            snapshotConfirmedMessage <-
              generate $
                genStateEvent $
                  Outcome.SnapshotConfirmed
                    { headId = testHeadId
                    , snapshot = Just snapshot
                    , signatures = mempty
                    }

            withClient port "/?snapshot-utxo=no" $ \conn -> do
              putEvent snapshotConfirmedMessage

              waitMatch 5 conn $ \v ->
                guard $ isNothing $ v ^? key "utxo"

    it "sequence numbers on history are based on the event id" $
      forAllShrink (listOf genStateEventForApi) shrink $ \events -> do
        monadicIO $ do
          run $
            -- NOTE: 20s, matching the similar property test above, so the
            -- timeout doesn't fire just because the full tasty suite is
            -- running many of these in parallel and starving each server's
            -- WS read of CPU.
            showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 20 $
              withFreeServerSocket $ \sock port ->
                withTestAPIServer sock port alice (mockSource events) tracer $ \_ -> do
                  withClient port "/?history=yes" $ \conn -> do
                    -- NOTE: Expect all history + greetings
                    received :: [ByteString] <- replicateM (length events + 1) (receiveData conn)
                    let seqs :: [Word64] = mapMaybe (\v -> v ^? key "seq" . _Number <&> truncate) received
                    seqs `shouldBe` getEventId <$> events

    it "displays correctly headStatus and snapshotUtxo in a Greeting message" $
      showLogsOnFailure "ServerSpec" $ \tracer ->
        withFreeServerSocket $ \sock port -> do
          -- Use a single headId throughout so the headId validation in
          -- 'aggregateNodeState' does not drop events from a mismatched head.
          headId <- generate arbitrary

          -- Prime some relevant server outputs already into event source to
          -- check whether the latest headStatus is loaded correctly.
          existingStateChanges <-
            generate $
              mapM
                (>>= genStateEvent)
                [ Outcome.HeadOpened <$> arbitrary <*> arbitrary <*> pure headId <*> arbitrary <*> arbitrary
                ]
          let eventSource = mockSource existingStateChanges

          withTestAPIServer sock port alice eventSource tracer $ \(EventSink{putEvent}, _) -> do
            let generateSnapshot =
                  (Outcome.SnapshotConfirmed headId . Just <$> arbitrary) <*> arbitrary

            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Open")
              guard $ v ^? key "snapshotUtxo" == Just (Aeson.Array mempty)

            snapShotConfirmedMsg@StateEvent{stateChanged = Outcome.SnapshotConfirmed{snapshot = Just Snapshot{utxo, utxoToCommit}}} <-
              generate $ genStateEvent =<< generateSnapshot

            putEvent snapShotConfirmedMsg
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Open")
              guard $ v ^? key "snapshotUtxo" == Just (toJSON $ utxo <> fromMaybe mempty utxoToCommit)

            snapShotConfirmedMsg'@StateEvent
              { stateChanged =
                Outcome.SnapshotConfirmed{snapshot = Just Snapshot{utxo = utxo', utxoToCommit = utxoToCommit'}}
              } <-
              generate $ genStateEvent =<< generateSnapshot
            headClosedMsg <-
              generate $
                genStateEvent
                  =<< ( Outcome.HeadClosed headId <$> arbitrary <*> arbitrary <*> arbitrary
                      )
            readyToFanoutMsg <- generate $ genStateEvent Outcome.HeadIsReadyToFanout{headId}

            mapM_ putEvent [snapShotConfirmedMsg', headClosedMsg, readyToFanoutMsg]
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "FanoutPossible")
              guard $ v ^? key "snapshotUtxo" == Just (toJSON $ utxo' <> fromMaybe mempty utxoToCommit')

    it "greets with correct head status and snapshot utxo after restart" $
      showLogsOnFailure "ServerSpec" $ \tracer ->
        withFreeServerSocket $ \sock port -> do
          headIsOpenMsg@Outcome.HeadOpened{headId = openedHeadId} <- generate $ Outcome.HeadOpened <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

          let generateSnapshot = generate $ (Outcome.SnapshotConfirmed openedHeadId . Just <$> arbitrary) <*> arbitrary
          snapShotConfirmedMsg@Outcome.SnapshotConfirmed{snapshot = Just Snapshot{utxo, utxoToCommit}} <- generateSnapshot

          stateEvents :: [StateEvent SimpleTx] <- generate $ mapM genStateEvent [headIsOpenMsg, snapShotConfirmedMsg]
          let eventSource = mockSource stateEvents

          let expectedUtxos = toJSON $ utxo <> fromMaybe mempty utxoToCommit
          withTestAPIServer sock port alice eventSource tracer $ \_ -> do
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Open")
              guard $ v ^? key "snapshotUtxo" == Just expectedUtxos

          withTestAPIServer sock port alice eventSource tracer $ \_ -> do
            waitForValue port $ \v -> do
              guard $ v ^? key "headStatus" == Just (Aeson.String "Open")
              guard $ v ^? key "snapshotUtxo" == Just expectedUtxos

    it "sends an error when input cannot be decoded" $
      failAfter 5 $
        withFreeServerSocket sendsAnErrorWhenInputCannotBeDecoded

    -- The snapshot decodes fine; it is forcing its
    -- accumulator that throws, and this server would do that while tracing the
    -- input -- on the log writer thread, taking logging and eventually the
    -- whole node with it. So the size has to be rejected as part of decoding.
    -- The assertion that matters here is that the connection survives and stays
    -- usable: a reply at all means nothing forced the accumulator.
    it "sends an error when a side-loaded snapshot exceeds the accumulator limit" $
      failAfter 5 $
        showLogsOnFailure "ServerSpec" $ \tracer ->
          withFreeServerSocket $ \sock port ->
            withTestAPIServer sock port alice (mockSource []) tracer $ \_ ->
              withClient port "/" $ \con -> do
                _greeting :: ByteString <- receiveData con
                signatures <- generate (arbitrary @(MultiSignature (Snapshot SimpleTx)))
                let bigCount = Accumulator.maxAccumulatorSize + 1
                    oversized =
                      Aeson.encode $
                        Aeson.object
                          [ "tag" .= Aeson.String "SideLoadSnapshot"
                          , "snapshot"
                              .= Aeson.object
                                [ "tag" .= Aeson.String "ConfirmedSnapshot"
                                , "snapshot"
                                    .= Aeson.object
                                      [ "headId" .= testHeadId
                                      , "version" .= (0 :: Int)
                                      , "number" .= (1 :: Int)
                                      , "confirmed" .= ([] :: [SimpleTx])
                                      , "utxo" .= utxoRefs [1 .. fromIntegral bigCount]
                                      ]
                                , "signatures" .= signatures
                                ]
                          ]
                sendBinaryData con oversized
                msg <- receiveData con
                case Aeson.eitherDecode @InvalidInput msg of
                  Left{} -> failure $ "Failed to decode output " <> show msg
                  Right InvalidInput{reason} ->
                    reason `shouldContain` show Accumulator.maxAccumulatorSize
                -- Still alive and still serving.
                sendBinaryData con ("not a valid message" :: ByteString)
                _ :: ByteString <- receiveData con
                pure ()

    describe "CBOR encoding" $ do
      it "sends a CBOR-encoded greeting when connecting with encoding=cbor" $
        failAfter 5 $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreeServerSocket $ \sock port ->
              withTestAPIServer sock port alice (mockSource []) tracer $ \_ ->
                withClient port "/?encoding=cbor&history=no" $ \conn -> do
                  bytes :: ByteString <- receiveData conn
                  case decodeFull' @(ApiMessage SimpleTx) bytes of
                    Left err -> failure $ "Failed to decode CBOR greeting: " <> show err
                    Right ApiGreetings{} -> pure ()
                    Right other -> failure $ "Expected ApiGreetings, but got: " <> show other

      it "sends server outputs CBOR-encoded to clients connected with encoding=cbor" $
        failAfter 5 $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreeServerSocket $ \sock port ->
              withTestAPIServer sock port alice (mockSource []) tracer $ \(EventSink{putEvent}, _) ->
                withClient port "/?encoding=cbor&history=no" $ \conn -> do
                  _greeting :: ByteString <- receiveData conn
                  arbitraryEvent <- generate genStateEventForApi
                  let expectedMessage =
                        fromMaybe (error "failed to convert stateEvent") $
                          mkTimedServerOutputFromStateEvent Nothing arbitraryEvent
                  putEvent arbitraryEvent
                  bytes :: ByteString <- receiveData conn
                  case decodeFull' @(ApiMessage SimpleTx) bytes of
                    Left err -> failure $ "Failed to decode CBOR server output: " <> show err
                    Right (ApiTimedServerOutput timedOutput) -> timedOutput `shouldBe` expectedMessage
                    Right other -> failure $ "Expected ApiTimedServerOutput, but got: " <> show other

      it "accepts CBOR-encoded client inputs when connected with encoding=cbor" $
        failAfter 5 $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreeServerSocket $ \sock port -> do
              inputs <- newLabelledTQueueIO "cbor-inputs"
              let recordInput = atomically . writeTQueue inputs
              withTestAPIServerWithCallback (Just sock) port alice (mockSource []) tracer recordInput $ \_ ->
                withClient port "/?encoding=cbor&history=no" $ \conn -> do
                  _greeting :: ByteString <- receiveData conn
                  sendBinaryData conn $ serialize' (Init :: ClientInput SimpleTx)
                  failAfter 10 $ atomically (readTQueue inputs) `shouldReturn` Init

      it "sends a CBOR-encoded InvalidInput when input is not valid CBOR" $
        failAfter 5 $
          showLogsOnFailure "ServerSpec" $ \tracer ->
            withFreeServerSocket $ \sock port ->
              withTestAPIServer sock port alice (mockSource []) tracer $ \_ ->
                withClient port "/?encoding=cbor&history=no" $ \conn -> do
                  _greeting :: ByteString <- receiveData conn
                  let garbage = "not a valid CBOR message" :: ByteString
                  sendBinaryData conn garbage
                  bytes :: ByteString <- receiveData conn
                  case decodeFull' @(ApiMessage SimpleTx) bytes of
                    Left err -> failure $ "Failed to decode CBOR InvalidInput: " <> show err
                    Right (ApiInvalidInput InvalidInput{input = echoed}) ->
                      echoed `shouldBe` encodeBase16 garbage
                    Right other -> failure $ "Expected ApiInvalidInput, but got: " <> show other

    -- The filter's own semantics live in 'Hydra.API.ServerOutputFilterSpec'.
    -- What matters here is the wiring that spec cannot reach: the address from
    -- the query string arriving at the filter, both output paths consulting
    -- it, and client messages bypassing it.
    describe "address filtering" $ do
      it "consults the filter with the address from the query string" $
        showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 20 $ do
          event <- generate genStateEventForApi
          withFreeServerSocket $ \sock port ->
            withTestAPIServerWithFilter sock port alice (mockSource []) (onlyAddress "addr_test1vp") tracer $ \(EventSink{putEvent}, _) ->
              withClient port "/?address=addr_test1vp" $ \matching ->
                withClient port "/?address=addr_test1vq" $ \other -> do
                  waitMatch 20 matching $ guard . matchGreetings
                  waitMatch 20 other $ guard . matchGreetings
                  putEvent event
                  waitMatch 20 matching $ guard . (== toJSON (timedOutputOf event))
                  receivesNothing other

      it "drops rejected outputs from the live stream" $
        showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 20 $ do
          event <- generate genStateEventForApi
          withFreeServerSocket $ \sock port ->
            withTestAPIServerWithFilter sock port alice (mockSource []) rejectEverything tracer $ \(EventSink{putEvent}, _) ->
              withClient port "/?address=addr_test1vp" $ \con -> do
                -- The greeting bypasses the filter, so it still arrives.
                waitMatch 20 con $ guard . matchGreetings
                putEvent event
                receivesNothing con

      -- Replayed history goes through 'forwardHistory', a call site separate
      -- from the live stream. It is forwarded BEFORE the greeting, so this has
      -- to assert the first frame: waiting for the greeting would drain the
      -- very output under test and pass whatever the filter does (see the same
      -- trap noted for "does not echo history if client says no").
      it "applies the filter to replayed history" $
        showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 20 $ do
          event <- generate genStateEventForApi
          withFreeServerSocket $ \sock port ->
            withTestAPIServerWithFilter sock port alice (mockSource [event]) allowEverythingServerOutputFilter tracer $ \_ ->
              withClient port "/?history=yes&address=addr_test1vp" $ \con ->
                nextFrame con `shouldReturn` toJSON (timedOutputOf event)
          withFreeServerSocket $ \sock port ->
            withTestAPIServerWithFilter sock port alice (mockSource [event]) rejectEverything tracer $ \_ ->
              withClient port "/?history=yes&address=addr_test1vp" $
                nextFrame >=> (`shouldSatisfy` matchGreetings)

      -- Without an address the filter must not be consulted at all, which is
      -- the branch every ordinary client takes.
      it "does not filter a client that gave no address" $
        showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 20 $ do
          event <- generate genStateEventForApi
          withFreeServerSocket $ \sock port ->
            withTestAPIServerWithFilter sock port alice (mockSource []) rejectEverything tracer $ \(EventSink{putEvent}, _) ->
              withClient port "/" $ \con -> do
                waitMatch 20 con $ guard . matchGreetings
                putEvent event
                waitMatch 20 con $ guard . (== toJSON (timedOutputOf event))

      -- A 'ClientMessage' carries no transaction to match an address against,
      -- so a filtered client must still receive it; otherwise an error or a
      -- rejected command would silently never reach it.
      it "never filters client messages" $
        showLogsOnFailure "ServerSpec" $ \tracer -> failAfter 20 $ do
          let message :: ClientMessage SimpleTx
              message = RejectedInputBecauseUnsynced{clientInput = Init, drift = 1}
          withFreeServerSocket $ \sock port ->
            withTestAPIServerWithFilter sock port alice (mockSource []) rejectEverything tracer $ \(_, server) ->
              withClient port "/?address=addr_test1vp" $ \con -> do
                waitMatch 20 con $ guard . matchGreetings
                sendMessage server message
                waitMatch 20 con $ guard . (== toJSON message)

    describe "connection query string" $ do
      let configFor = mkServerOutputConfig . queryParamsOf
          addressIn = addressInTx . configFor
          utxoIn = utxoInSnapshot . configFor
          encodingIn = encoding . configFor

      it "filters on the given address" $
        addressIn "/?address=addr_test1vp" `shouldBe` WithAddressedTx "addr_test1vp"

      -- An empty filter would match nothing, since addresses are compared
      -- exactly, and the client would silently see no transaction outputs.
      it "ignores an address without a value" $ do
        addressIn "/?address=" `shouldBe` WithoutAddressedTx
        addressIn "/?address" `shouldBe` WithoutAddressedTx

      it "skips valueless addresses in favour of a later one" $
        addressIn "/?address&address=addr_test1vp" `shouldBe` WithAddressedTx "addr_test1vp"

      it "omits the snapshot utxo on request" $ do
        utxoIn "/?snapshot-utxo=no" `shouldBe` WithoutUTxO
        utxoIn "/" `shouldBe` WithUTxO

      it "switches to CBOR on request" $ do
        encodingIn "/?encoding=cbor" `shouldBe` CborEncoding
        encodingIn "/" `shouldBe` JsonEncoding

      -- Replay is opt-in: only an explicit 'yes' turns it on, so 'history=no'
      -- and no parameter at all behave identically.
      it "serves history on request" $ do
        shouldServeHistory (queryParamsOf "/?history=yes") `shouldBe` True
        shouldServeHistory (queryParamsOf "/?history=no") `shouldBe` False
        shouldServeHistory (queryParamsOf "/") `shouldBe` False

      -- A malformed query used to raise a parse exception during connection
      -- setup; now it just leaves the client with the defaults.
      it "falls back to the defaults on a malformed query" $
        configFor "/?%%&=&address"
          `shouldBe` ServerOutputConfig{utxoInSnapshot = WithUTxO, addressInTx = WithoutAddressedTx, encoding = JsonEncoding}

    describe "TLS support" $ do
      it "accepts TLS connections when configured" $ do
        showLogsOnFailure "ServerSpec" $ \tracer ->
          withFreeServerSocket $ \sock port -> do
            let config =
                  APIServerConfig
                    { host = "127.0.0.1"
                    , port
                    , tlsCertPath = Just "test/tls/certificate.pem"
                    , tlsKeyPath = Just "test/tls/key.pem"
                    , apiTransactionTimeout = 1000000
                    , listenSocket = Just sock
                    }
                initialChainState = 0
            withAPIServer @SimpleTx config defaultRunOptions testEnvironment alice (mockSource []) tracer initialChainState dummyChainHandle defaultPParams allowEverythingServerOutputFilter noop $ \_ -> do
              let clientParams = defaultParamsClient "127.0.0.1" ""
                  allowAnyParams =
                    clientParams{clientHooks = (clientHooks clientParams){onServerCertificate = \_ _ _ _ -> pure []}}
              WSS.connect allowAnyParams "127.0.0.1" (show port) "/" [] $ \(conn, _) -> do
                waitMatch 5 conn $ guard . matchGreetings

    -- 'mkTimedServerOutputFromStateEvent' decides which internal state changes
    -- reach clients and as what. Every other test of the output pipeline
    -- computes its expectation by calling that same function, so it cannot
    -- catch a mapping that sends the wrong output or silently stops surfacing
    -- one. Hence a table naming each translation, checked against the
    -- constructors that actually exist.
    describe "state change to client output" $ do
      it "classifies exactly the state changes that exist" $ do
        actual <- stateChangeConstructors
        sort (fst <$> surfacedOutputs) `shouldBe` sort actual

      it "translates each state change to its expected output" $ do
        samples <- sampleStateChanges
        forM_ samples $ \(name, stateChanged) -> do
          expected <-
            maybe (failure $ "state change " <> name <> " is not in surfacedOutputs") pure $
              List.lookup name surfacedOutputs
          event <- generate $ genStateEvent stateChanged
          let actual = outputTagOf <$> mkTimedServerOutputFromStateEvent (Just aSeenSnapshot) event
          unless (actual == expected) . failure $
            name <> ": expected " <> show expected <> " but got " <> show actual

      -- The table above compares tags, which cannot see inside a payload. These
      -- are the three arms that rename or derive a field rather than copying
      -- it, so they are where a wrong wiring survives a matching tag.
      it "does not swap the two UTxO sets of a partial fanout" $ do
        let distributed = utxoRefs [1, 2]
            remaining = utxoRefs [3]
        event <-
          generate . genStateEvent $
            Outcome.HeadPartialFannedOut
              { headId = testHeadId
              , distributedOutputs = distributed
              , remainingOutputs = remaining
              , chainState = 0
              , mode = AutoDrain
              }
        case output <$> mkTimedServerOutputFromStateEvent Nothing event of
          Just HeadPartiallyFannedOut{distributedUTxO, remainingUTxO} -> do
            distributedUTxO `shouldBe` distributed
            remainingUTxO `shouldBe` remaining
          other -> failure $ "expected HeadPartiallyFannedOut, got " <> show other

      it "reports the transaction id of an applied transaction" $ do
        let tx = aValidTx 42
        event <-
          generate . genStateEvent $
            Outcome.TransactionAppliedToLocalUTxO{headId = testHeadId, tx}
        case output <$> mkTimedServerOutputFromStateEvent Nothing event of
          Just TxValid{transactionId} -> transactionId `shouldBe` txId tx
          other -> failure $ "expected TxValid, got " <> show other

      it "derives the decommitted UTxO from the decommit transaction" $ do
        let decommitTx = aValidTx 42
        event <-
          generate . genStateEvent $
            Outcome.DecommitRecorded{headId = testHeadId, decommitTx}
        case output <$> mkTimedServerOutputFromStateEvent Nothing event of
          Just DecommitRequested{utxoToDecommit} -> utxoToDecommit `shouldBe` utxoFromTx decommitTx
          other -> failure $ "expected DecommitRequested, got " <> show other

      -- Why the seen-snapshot argument exists: on the normal signing path a
      -- 'SnapshotConfirmed' carries no snapshot of its own, so with none seen
      -- there is nothing to send and the client hears nothing at all.
      it "cannot surface a confirmed snapshot it has never seen" $ do
        event <-
          generate . genStateEvent $
            Outcome.SnapshotConfirmed{headId = testHeadId, snapshot = Nothing, signatures = mempty}
        mkTimedServerOutputFromStateEvent Nothing event `shouldSatisfy` isNothing
        outputTagOf
          <$> mkTimedServerOutputFromStateEvent (Just aSeenSnapshot) event
            `shouldBe` Just "SnapshotConfirmed"

    -- Read models served by the HTTP API. A wrong arm in either fails
    -- silently: deposits stop being draftable, or 'Greetings' reports the
    -- wrong connectivity.
    describe "projectCommitInfo" $ do
      it "allows committing once the head is open" $
        projectCommitInfo CannotCommit anOpenedHead `shouldBe` IncrementalCommit testHeadId

      it "stops allowing commits once the head is closed" $
        projectCommitInfo (IncrementalCommit testHeadId) aClosedHead `shouldBe` CannotCommit

      -- A rotated event log replays as a single 'Checkpoint', so the head id
      -- has to be recoverable from it rather than only from 'HeadOpened'.
      it "recovers the head id from a checkpoint of an open head" $
        projectCommitInfo CannotCommit (Outcome.Checkpoint $ inOpenState [alice])
          `shouldBe` IncrementalCommit testHeadId

      it "does not allow commits from a checkpoint of a head that is not open" $
        projectCommitInfo (IncrementalCommit testHeadId) (Outcome.Checkpoint inIdleState)
          `shouldBe` CannotCommit

      -- Seeded from both verdicts: an arm that sets the value it already
      -- holds is invisible from one side only.
      it "leaves the decision untouched for every unrelated state change" $ do
        others <- stateChangesOtherThan ["Checkpoint", "HeadOpened", "HeadClosed"]
        forM_ others $ \(name, stateChanged) ->
          forM_ [CannotCommit, IncrementalCommit testHeadId] $ \priorValue ->
            unless (projectCommitInfo priorValue stateChanged == priorValue) . failure $
              name <> " changed the commit info from " <> show priorValue

    describe "projectNetworkInfo" $ do
      it "records the network as connected and disconnected" $ do
        networkConnected (projectNetworkInfo disconnected Outcome.NetworkConnected) `shouldBe` True
        networkConnected (projectNetworkInfo connected Outcome.NetworkDisconnected) `shouldBe` False

      -- Peers are only known through the network, so a disconnect has to clear
      -- them rather than leave stale ones behind.
      it "forgets all peers when the network disconnects" $
        peersInfo (projectNetworkInfo connectedTo Outcome.NetworkDisconnected) `shouldBe` mempty

      it "tracks each peer separately" $ do
        let afterBoth =
              projectNetworkInfo connected Outcome.PeerConnected{peer = peerA}
                & flip projectNetworkInfo Outcome.PeerConnected{peer = peerB}
                & flip projectNetworkInfo Outcome.PeerDisconnected{peer = peerB}
        peersInfo afterBoth `shouldBe` Map.fromList [(peerA, True), (peerB, False)]

      it "leaves the info untouched for every unrelated state change" $ do
        others <-
          stateChangesOtherThan
            ["NetworkConnected", "NetworkDisconnected", "PeerConnected", "PeerDisconnected"]
        forM_ others $ \(name, stateChanged) ->
          forM_ [connectedTo, disconnected] $ \priorValue ->
            unless (projectNetworkInfo priorValue stateChanged == priorValue) . failure $
              name <> " changed the network info from " <> show priorValue

-- * State change translation fixtures

-- | Which client output each 'StateChanged' is surfaced as, by constructor
-- name; 'Nothing' for the ones deliberately kept internal. Adding a state
-- change without classifying it here fails
-- "classifies exactly the state changes that exist".
surfacedOutputs :: [(String, Maybe Text)]
surfacedOutputs =
  [ ("HeadOpened", Just "HeadIsOpen")
  , ("HeadClosed", Just "HeadIsClosed")
  , ("HeadContested", Just "HeadIsContested")
  , ("HeadIsReadyToFanout", Just "ReadyToFanout")
  , ("HeadFannedOut", Just "HeadIsFinalized")
  , ("HeadPartialFannedOut", Just "HeadPartiallyFannedOut")
  , ("HeadFanoutInitiated", Nothing)
  , ("HeadPartialFanoutSelected", Nothing)
  , ("HeadFanoutReverted", Nothing)
  , ("TransactionAppliedToLocalUTxO", Just "TxValid")
  , ("TxInvalid", Just "TxInvalid")
  , ("SnapshotConfirmed", Just "SnapshotConfirmed")
  , ("IgnoredHeadInitializing", Just "IgnoredHeadInitializing")
  , ("DecommitRecorded", Just "DecommitRequested")
  , ("DecommitInvalid", Just "DecommitInvalid")
  , ("DecommitApproved", Just "DecommitApproved")
  , ("DecommitFinalized", Just "DecommitFinalized")
  , ("DepositRecorded", Just "CommitRecorded")
  , ("DepositActivated", Just "DepositActivated")
  , ("DepositExpired", Just "DepositExpired")
  , ("DepositRecovered", Just "CommitRecovered")
  , ("CommitApproved", Just "CommitApproved")
  , ("CommitFinalized", Just "CommitFinalized")
  , ("NetworkConnected", Just "NetworkConnected")
  , ("NetworkDisconnected", Just "NetworkDisconnected")
  , ("NetworkVersionMismatch", Just "NetworkVersionMismatch")
  , ("NetworkClusterIDMismatch", Just "NetworkClusterIDMismatch")
  , ("PeerConnected", Just "PeerConnected")
  , ("PeerDisconnected", Just "PeerDisconnected")
  , ("TransactionReceived", Nothing)
  , ("SnapshotRequested", Nothing)
  , ("SnapshotRequestDecided", Nothing)
  , ("PartySignedSnapshot", Nothing)
  , ("ChainRolledBack", Nothing)
  , ("TickObserved", Nothing)
  , ("LocalStateCleared", Just "SnapshotSideLoaded")
  , ("Checkpoint", Just "EventLogRotated")
  , ("NodeUnsynced", Just "NodeUnsynced")
  , ("NodeSynced", Just "NodeSynced")
  ]

-- | One sample value per 'StateChanged' constructor, paired with its name.
sampleStateChanges :: IO [(String, Outcome.StateChanged SimpleTx)]
sampleStateChanges = do
  ADTArbitrary{adtCAPs} <- generate $ toADTArbitrary (Proxy @(Outcome.StateChanged SimpleTx))
  pure [(capConstructor, capArbitrary) | ConstructorArbitraryPair{capConstructor, capArbitrary} <- adtCAPs]

stateChangeConstructors :: IO [String]
stateChangeConstructors = fmap fst <$> sampleStateChanges

-- | Samples for every constructor except the named ones.
stateChangesOtherThan :: [String] -> IO [(String, Outcome.StateChanged SimpleTx)]
stateChangesOtherThan handled =
  filter ((`notElem` handled) . fst) <$> sampleStateChanges

outputTagOf :: TimedServerOutput SimpleTx -> Text
outputTagOf output =
  fromMaybe "<untagged>" $ toJSON output ^? key "tag" . _String

aSeenSnapshot :: Snapshot SimpleTx
aSeenSnapshot = testSnapshot 1 0 [] mempty

anOpenedHead :: Outcome.StateChanged SimpleTx
anOpenedHead =
  Outcome.HeadOpened
    { parameters = generateWith arbitrary 42
    , chainState = 0
    , headId = testHeadId
    , headSeed = generateWith arbitrary 42
    , parties = [alice]
    }

aClosedHead :: Outcome.StateChanged SimpleTx
aClosedHead =
  Outcome.HeadClosed
    { headId = testHeadId
    , snapshotNumber = 1
    , chainState = 0
    , contestationDeadline = generateWith arbitrary 42
    }

peerA, peerB :: Host
peerA = Host "10.0.0.1" 5001
peerB = Host "10.0.0.2" 5002

connected, disconnected, connectedTo :: NetworkInfo
connected = NetworkInfo{networkConnected = True, peersInfo = mempty}
disconnected = NetworkInfo{networkConnected = False, peersInfo = mempty}
connectedTo = connected{peersInfo = Map.fromList [(peerA, True)]}

sendsAnErrorWhenInputCannotBeDecoded :: Socket -> PortNumber -> Expectation
sendsAnErrorWhenInputCannotBeDecoded sock port = do
  showLogsOnFailure "ServerSpec" $ \tracer ->
    withTestAPIServer sock port alice (mockSource []) tracer $ \_ -> do
      withClient port "/" $ \con -> do
        _greeting :: ByteString <- receiveData con
        sendBinaryData con invalidInput
        msg <- receiveData con
        case Aeson.eitherDecode @InvalidInput msg of
          Left{} -> failure $ "Failed to decode output " <> show msg
          Right resp ->
            resp `shouldSatisfy` \case
              InvalidInput{input} -> input == invalidInput
 where
  invalidInput = "not a valid message"

matchGreetings :: Aeson.Value -> Bool
matchGreetings v =
  isJust (v ^? key "headStatus")
    && isJust (v ^? key "hydraNodeVersion")
    && isJust (v ^? key "me")

waitForClients :: (MonadSTM m, Ord a, Num a) => TVar m a -> m ()
waitForClients semaphore = atomically $ readTVar semaphore >>= \n -> check (n >= 2)

-- NOTE: this client runs indefinitely so it should be run within a context that won't
-- leak runaway threads
testClient :: TQueue IO Value -> TVar IO Int -> Connection -> IO ()
testClient queue semaphore cnx = do
  atomically $ modifyTVar' semaphore (+ 1)
  msg <- receiveData cnx
  case Aeson.eitherDecode msg of
    Left{} -> failure $ "Failed to decode message " <> show msg
    Right value -> do
      atomically (writeTQueue queue value)
      testClient queue semaphore cnx

dummyChainHandle :: Chain tx IO
dummyChainHandle =
  Chain
    { postTx = \_ -> error "unexpected call to postTx"
    , draftDepositTx = \_ -> error "unexpected call to draftDepositTx"
    , submitTx = \_ -> error "unexpected call to submitTx"
    , checkNonADAAssets = \_ -> error "unexpected call to checkNonADAAssets"
    }

allowEverythingServerOutputFilter :: ServerOutputFilter tx
allowEverythingServerOutputFilter =
  ServerOutputFilter
    { txContainsAddr = \_ _ -> True
    }

-- | Rejects every output, so anything a client still receives reached it
-- without passing the filter.
rejectEverything :: ServerOutputFilter tx
rejectEverything =
  ServerOutputFilter
    { txContainsAddr = \_ _ -> False
    }

-- | Accepts outputs only for one address, so which address the server asked
-- about is observable from which client receives the output.
onlyAddress :: Text -> ServerOutputFilter tx
onlyAddress wanted =
  ServerOutputFilter
    { txContainsAddr = \_ addr -> addr == wanted
    }

-- | The 'TimedServerOutput' a client is expected to receive for an event.
timedOutputOf :: StateEvent SimpleTx -> TimedServerOutput SimpleTx
timedOutputOf event =
  fromMaybe (error "event does not map to a server output") $
    mkTimedServerOutputFromStateEvent Nothing event

-- | The next frame on a connection, without skipping over any.
nextFrame :: HasCallStack => Connection -> IO Aeson.Value
nextFrame con = do
  bytes <- receiveData con
  case Aeson.eitherDecode' bytes of
    Left err -> failure $ "nextFrame failed to decode: " <> err
    Right value -> pure value

-- | Assert that nothing more arrives on a connection. Used where the
-- expectation is an absence, so it has to wait out a full second rather than
-- return on a match.
receivesNothing :: HasCallStack => Connection -> Expectation
receivesNothing con =
  timeout 1 (receiveData con) >>= \case
    Nothing -> pure ()
    Just (msg :: LByteString) ->
      failure $ "expected no further message, but received: " <> show msg

noop :: Applicative m => a -> m ()
noop = const $ pure ()

-- | Allocate a listening socket on a free port and hand both it and its port
-- to the action. The server is then given the very socket it serves on, so the
-- port cannot be taken in the gap between choosing it and binding it. That gap
-- used to surface as a flaky 'RunServerException' carrying
-- "Address already in use".
withFreeServerSocket :: (Socket -> PortNumber -> IO a) -> IO a
withFreeServerSocket action =
  bracket Warp.openFreePort (close . snd) $ \(p, sock) ->
    action sock (fromIntegral p)

withTestAPIServer ::
  Socket ->
  PortNumber ->
  Party ->
  EventSource (StateEvent SimpleTx) IO ->
  Tracer IO APIServerLog ->
  ((EventSink (StateEvent SimpleTx) IO, Server SimpleTx IO) -> IO ()) ->
  IO ()
withTestAPIServer sock port actor eventSource tracer =
  withTestAPIServerWithCallback (Just sock) port actor eventSource tracer noop

-- | Like 'withTestAPIServer' but lets the server bind @port@ itself. Only for
-- the test that asserts a second server on the same port is refused: handing
-- both servers one socket would let them both succeed.
withTestAPIServerBindingPort ::
  PortNumber ->
  Party ->
  EventSource (StateEvent SimpleTx) IO ->
  Tracer IO APIServerLog ->
  ((EventSink (StateEvent SimpleTx) IO, Server SimpleTx IO) -> IO ()) ->
  IO ()
withTestAPIServerBindingPort port actor eventSource tracer =
  withTestAPIServerWithCallback Nothing port actor eventSource tracer noop

-- | Like 'withTestAPIServer', but with an explicit callback invoked for every
-- 'ClientInput' received by the server.
withTestAPIServerWithCallback ::
  Maybe Socket ->
  PortNumber ->
  Party ->
  EventSource (StateEvent SimpleTx) IO ->
  Tracer IO APIServerLog ->
  (ClientInput SimpleTx -> IO ()) ->
  ((EventSink (StateEvent SimpleTx) IO, Server SimpleTx IO) -> IO ()) ->
  IO ()
withTestAPIServerWithCallback listenSocket port actor eventSource tracer =
  withTestAPIServer' listenSocket port actor eventSource allowEverythingServerOutputFilter tracer

-- | Like 'withTestAPIServer', but with an explicit 'ServerOutputFilter'.
withTestAPIServerWithFilter ::
  Socket ->
  PortNumber ->
  Party ->
  EventSource (StateEvent SimpleTx) IO ->
  ServerOutputFilter SimpleTx ->
  Tracer IO APIServerLog ->
  ((EventSink (StateEvent SimpleTx) IO, Server SimpleTx IO) -> IO ()) ->
  IO ()
withTestAPIServerWithFilter sock port actor eventSource outputFilter tracer =
  withTestAPIServer' (Just sock) port actor eventSource outputFilter tracer noop

withTestAPIServer' ::
  Maybe Socket ->
  PortNumber ->
  Party ->
  EventSource (StateEvent SimpleTx) IO ->
  ServerOutputFilter SimpleTx ->
  Tracer IO APIServerLog ->
  (ClientInput SimpleTx -> IO ()) ->
  ((EventSink (StateEvent SimpleTx) IO, Server SimpleTx IO) -> IO ()) ->
  IO ()
withTestAPIServer' listenSocket port actor eventSource outputFilter tracer =
  withAPIServer @SimpleTx config defaultRunOptions testEnvironment actor eventSource tracer 0 dummyChainHandle defaultPParams outputFilter
 where
  config = APIServerConfig{host = "127.0.0.1", port, tlsCertPath = Nothing, tlsKeyPath = Nothing, apiTransactionTimeout = 1000000, listenSocket}

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
  withClient port "/?history=no" $ \conn ->
    waitMatch 5 conn f

-- | Wait up to some time for an API server output to match the given predicate.
waitMatch :: HasCallStack => Natural -> Connection -> (Aeson.Value -> Maybe a) -> IO a
waitMatch delay con match = do
  seenMsgs <- newLabelledTVarIO "wait-match-seen-msgs" []
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

  waitNext :: Connection -> IO Value
  waitNext connection = do
    bytes <- receiveData connection
    case Aeson.eitherDecode' bytes of
      Left err -> failure $ "WaitNext failed to decode msg: " <> err
      Right value -> pure value

shouldSatisfyAll :: forall a. HasCallStack => Show a => [a] -> [a -> Bool] -> Expectation
shouldSatisfyAll = go
 where
  go :: [a] -> [a -> Bool] -> IO ()
  go [] [] = pure ()
  go [] _ = failure "shouldSatisfyAll: ran out of values"
  go _ [] = failure "shouldSatisfyAll: ran out of predicates"
  go (v : vs) (p : ps) = do
    v `shouldSatisfy` p
    go vs ps

genStateEventForApi :: Gen (StateEvent SimpleTx)
genStateEventForApi =
  arbitrary `suchThat` (isJust . mkTimedServerOutputFromStateEvent Nothing)
