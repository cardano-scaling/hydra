module Hydra.API.HTTPServerSpec where

import Hydra.Prelude hiding (delete, get)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.STM (newTChanIO, writeTChan)
import Control.Lens ((^?))
import Data.Aeson (Result (Error, Success), eitherDecode, encode, fromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, nth)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.HTTPServer (
  DraftCommitTxRequest (..),
  DraftCommitTxResponse (..),
  SideLoadSnapshotRequest (..),
  SubmitL2TxRequest (..),
  SubmitL2TxResponse (..),
  SubmitTxRequest (..),
  TransactionSubmitted,
  httpApp,
 )
import Hydra.API.ServerOutput (ClientMessage (..), CommitInfo (..), DecommitInvalidReason (..), ServerOutput (..), TimedServerOutput (..), getConfirmedSnapshot, getSeenSnapshot, getSnapshotUtxo)
import Hydra.API.ServerSpec (dummyChainHandle, dummySimpleChainHandle, dummyUnsafeChainHandle)
import Hydra.Cardano.Api (
  UTxO,
  mkTxOutDatumInline,
  modifyTxOutDatum,
  renderTxIn,
  serialiseToTextEnvelope,
 )
import Hydra.Chain (Chain (draftCommitTx), PostTxError (..), draftDepositTx)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.Chain.Direct.Handlers (rejectLowDeposits)
import Hydra.HeadLogic.State (ClosedState (..), HeadState (..), SeenSnapshot (..))
import Hydra.HeadLogicSpec (inIdleState, inUnsyncedIdleState)
import Hydra.JSONSchema (SchemaSelector, prop_validateJSONSchema, validateJSON, withJsonSpecifications)
import Hydra.Ledger (ValidationError (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (nullTracer)
import Hydra.Node.State (NodeState (..))
import Hydra.Tx (ConfirmedSnapshot (..))
import Hydra.Tx.IsTx (UTxOType, txId)
import Hydra.Tx.Snapshot (Snapshot (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec.Wai (MatchBody (..), ResponseMatcher (matchBody), delete, get, post, shouldRespondWith, with)
import Test.Hspec.Wai.Internal (withApplication)
import Test.Hydra.Node.Fixture (testEnvironment)
import Test.Hydra.Tx.Fixture (defaultPParams, pparams)
import Test.Hydra.Tx.Gen (genTxOut, genUTxOAdaOnlyOfSize)
import Test.QuickCheck (
  checkCoverage,
  counterexample,
  cover,
  forAll,
  generate,
  property,
  withMaxSuccess,
 )

dummyStatePath :: FilePath
dummyStatePath = "~"

spec :: Spec
spec = do
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (DraftCommitTxResponse Tx)))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (DraftCommitTxRequest Tx)))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (SubmitTxRequest Tx)))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized TransactionSubmitted))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (SideLoadSnapshotRequest Tx)))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (HeadState Tx)))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (NodeState Tx)))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized SubmitL2TxResponse))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (SubmitL2TxRequest Tx)))

    prop "Validate /commit publish api schema" $
      prop_validateJSONSchema @(DraftCommitTxRequest Tx) "api.json" $
        key "components" . key "messages" . key "DraftCommitTxRequest" . key "payload"

    prop "Validate /commit subscribe api schema" $
      prop_validateJSONSchema @(DraftCommitTxResponse Tx) "api.json" $
        key "components" . key "messages" . key "DraftCommitTxResponse" . key "payload"

    prop "Validate /cardano-transaction publish api schema" $
      prop_validateJSONSchema @(SubmitTxRequest Tx) "api.json" $
        key "channels"
          . key "/cardano-transaction"
          . key "publish"
          . key "message"
          . key "payload"

    prop "Validate /cardano-transaction subscribe api schema" $
      prop_validateJSONSchema @TransactionSubmitted "api.json" $
        key "channels"
          . key "/cardano-transaction"
          . key "subscribe"
          . key "message"
          . key "oneOf"
          . nth 0
          . key "payload"

    prop "Validate /decommit publish api schema" $
      prop_validateJSONSchema @Tx "api.json" $
        key "channels"
          . key "/decommit"
          . key "publish"
          . key "message"

    prop "Validate /decommit subscribe api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/decommit"
          . key "subscribe"
          . key "message"

    prop "Validate /commit publish api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/commit"
          . key "publish"
          . key "message"

    prop "Validate /commit subscribe api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/commit"
          . key "subscribe"
          . key "message"

    prop "Validate /commits publish api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/commits"
          . key "publish"
          . key "message"

    prop "Validate /commits subscribe api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/commits"
          . key "subscribe"
          . key "message"

    prop "Validate /commits/tx-id publish api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/commits/tx-id"
          . key "publish"
          . key "message"

    prop "Validate /commits/tx-id subscribe api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/commits/tx-id"
          . key "subscribe"
          . key "message"

    prop "Validate /snapshot publish api schema" $
      prop_validateJSONSchema @(SideLoadSnapshotRequest Tx) "api.json" $
        key "components" . key "messages" . key "SideLoadSnapshotRequest" . key "payload"

    prop "Validate /snapshot subscribe api schema" $
      prop_validateJSONSchema @(ConfirmedSnapshot Tx) "api.json" $
        key "components" . key "schemas" . key "ConfirmedSnapshot"

    prop "Validate /head publish api schema" $
      prop_validateJSONSchema @Text "api.json" $
        key "channels"
          . key "/head"
          . key "publish"
          . key "message"

    prop "Validate /head subscribe api schema" $
      prop_validateJSONSchema @(HeadState Tx) "api.json" $
        key "channels"
          . key "/head"
          . key "subscribe"
          . key "message"

    prop "Validate /transaction publish api schema" $
      prop_validateJSONSchema @(SubmitL2TxRequest Tx) "api.json" $
        key "channels"
          . key "/transaction"
          . key "publish"
          . key "message"
          . key "payload"

    prop "Validate /transaction subscribe api schema" $
      prop_validateJSONSchema @SubmitL2TxResponse "api.json" $
        key "components"
          . key "schemas"
          . key "SubmitL2TxResponse"

    apiServerSpec
    describe "SubmitTxRequest accepted tx formats" $ do
      prop "accepts json encoded transaction" $
        forAll (arbitrary @Tx) $ \tx ->
          let json = toJSON tx
           in case fromJSON @(SubmitTxRequest Tx) json of
                Success{} -> property True
                Error e -> counterexample (toString $ toText e) $ property False
      prop "accepts transaction encoded as TextEnvelope" $
        forAll (arbitrary @Tx) $ \tx ->
          let json = toJSON $ serialiseToTextEnvelope Nothing tx
           in case fromJSON @(SubmitTxRequest Tx) json of
                Success{} -> property True
                Error e -> counterexample (toString $ toText e) $ property False

apiServerSpec :: Spec
apiServerSpec = do
  describe "API should respond correctly" $ do
    let cantCommit = pure CannotCommit
        getPendingDeposits :: IO [tx]
        getPendingDeposits = pure []

        putClientInput :: ClientInput tx -> IO ()
        putClientInput = const (pure ())
        getNodeState = pure inIdleState
    describe "GET /protocol-parameters" $ do
      responseChannel <- runIO newTChanIO
      with
        ( return $
            httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              getNodeState
              cantCommit
              getPendingDeposits
              putClientInput
              300
              responseChannel
        )
        $ do
          it "matches schema" $
            withJsonSpecifications $ \schemaDir -> do
              get "/protocol-parameters"
                `shouldRespondWith` 200
                  { matchBody =
                      matchValidJSON
                        (schemaDir </> "api.json")
                        (key "components" . key "messages" . key "ProtocolParameters" . key "payload")
                  }
          it "responds given parameters" $
            get "/protocol-parameters"
              `shouldRespondWith` 200
                { matchBody = matchJSON defaultPParams
                }

    describe "GET /head" $ do
      responseChannel <- runIO newTChanIO
      prop "responds correctly" $ \nodeState -> do
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure nodeState)
              cantCommit
              getPendingDeposits
              putClientInput
              300
              responseChannel
          )
          $ do
            get "/head"
              `shouldRespondWith` 200{matchBody = matchJSON (headState nodeState)}
      responseChannelSimpleTx <- runIO newTChanIO
      prop "ok response matches schema" $ \nodeState -> do
        let isIdle = case headState nodeState of
              Idle{} -> True
              _ -> False
        let isInitial = case headState nodeState of
              Initial{} -> True
              _ -> False
        let isOpen = case headState nodeState of
              Open{} -> True
              _ -> False
        let isClosed = case headState nodeState of
              Closed{} -> True
              _ -> False
        withMaxSuccess 20
          . cover 1 isIdle "IdleState"
          . cover 1 isInitial "InitialState"
          . cover 1 isOpen "OpenState"
          . cover 1 isClosed "ClosedState"
          . withJsonSpecifications
          $ \schemaDir -> do
            withApplication
              ( httpApp @Tx
                  nullTracer
                  dummyChainHandle
                  testEnvironment
                  dummyStatePath
                  defaultPParams
                  (pure nodeState)
                  cantCommit
                  getPendingDeposits
                  putClientInput
                  300
                  responseChannelSimpleTx
              )
              $ do
                get "/head"
                  `shouldRespondWith` 200
                    { matchBody =
                        matchValidJSON
                          (schemaDir </> "api.json")
                          (key "channels" . key "/head" . key "subscribe" . key "message")
                    }
    describe "GET /snapshot/last-seen" $ do
      responseChannel <- runIO newTChanIO
      prop "responds correctly" $ \nodeState -> do
        let seenSnapshot :: SeenSnapshot SimpleTx = getSeenSnapshot (headState nodeState)
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure nodeState)
              cantCommit
              getPendingDeposits
              putClientInput
              300
              responseChannel
          )
          $ do
            get "/snapshot/last-seen"
              `shouldRespondWith` 200{matchBody = matchJSON seenSnapshot}
    describe "GET /snapshot" $ do
      responseChannel <- runIO newTChanIO
      prop "responds correctly" $ \nodeState -> do
        let confirmedSnapshot :: Maybe (ConfirmedSnapshot SimpleTx) = getConfirmedSnapshot (headState nodeState)
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure nodeState)
              cantCommit
              getPendingDeposits
              putClientInput
              300
              responseChannel
          )
          $ do
            get "/snapshot"
              `shouldRespondWith` case confirmedSnapshot of
                Nothing -> 404
                Just confirmedSn -> 200{matchBody = matchJSON confirmedSn}
      responseChannelSimpleTx <- runIO newTChanIO
      prop "ok response matches schema" $ \(closedState :: ClosedState tx) -> do
        withMaxSuccess 4
          . withJsonSpecifications
          $ \schemaDir -> do
            withApplication
              ( httpApp @Tx
                  nullTracer
                  dummyChainHandle
                  testEnvironment
                  dummyStatePath
                  defaultPParams
                  (pure NodeInSync{headState = Closed closedState, pendingDeposits = mempty, currentSlot = ChainSlot 0})
                  cantCommit
                  getPendingDeposits
                  putClientInput
                  300
                  responseChannelSimpleTx
              )
              $ do
                get "/snapshot"
                  `shouldRespondWith` 200
                    { matchBody =
                        matchValidJSON
                          (schemaDir </> "api.json")
                          (key "channels" . key "/snapshot" . key "subscribe" . key "message" . key "payload")
                    }

    describe "POST /snapshot" $ do
      it "returns 202 on timeout" $ do
        responseChannel <- newTChanIO
        let reqGen = generate (arbitrary @(SideLoadSnapshotRequest SimpleTx))
        request <- reqGen
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              cantCommit
              getPendingDeposits
              putClientInput
              0
              responseChannel
          )
          $ do
            post "/snapshot" (Aeson.encode request) `shouldRespondWith` 202

      it "returns 200 on SnapshotSideLoaded" $ do
        responseChannel <- newTChanIO
        let reqGen = generate (arbitrary @(SideLoadSnapshotRequest SimpleTx))
        request <- reqGen
        now' <- getCurrentTime
        let event =
              TimedServerOutput
                { output = SnapshotSideLoaded{headId = generateWith arbitrary 42, snapshotNumber = 7}
                , seq = 0
                , time = now'
                }
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              cantCommit
              getPendingDeposits
              (const $ atomically $ writeTChan responseChannel (Left event))
              10
              responseChannel
          )
          $ do
            post "/snapshot" (Aeson.encode request) `shouldRespondWith` 200

      it "returns 400 on CommandFailed" $ do
        responseChannel <- newTChanIO
        let reqGen = generate (arbitrary @(SideLoadSnapshotRequest SimpleTx))
        SideLoadSnapshotRequest snapshot <- reqGen
        let clientFailed = Right (CommandFailed{clientInput = SideLoadSnapshot snapshot, state = headState inIdleState})
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              cantCommit
              getPendingDeposits
              (const $ atomically $ writeTChan responseChannel clientFailed)
              10
              responseChannel
          )
          $ do
            post "/snapshot" (Aeson.encode (SideLoadSnapshotRequest snapshot)) `shouldRespondWith` 400

      it "returns 503 on RejectedInput" $ do
        responseChannel <- newTChanIO
        let reqGen = generate (arbitrary @(SideLoadSnapshotRequest SimpleTx))
        SideLoadSnapshotRequest snapshot <- reqGen
        let clientFailed = RejectedInput{clientInput = SideLoadSnapshot snapshot, reason = "chain out of sync"}
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inUnsyncedIdleState)
              cantCommit
              getPendingDeposits
              (const $ atomically $ writeTChan responseChannel (Right clientFailed))
              10
              responseChannel
          )
          $ do
            post "/snapshot" (Aeson.encode (SideLoadSnapshotRequest snapshot)) `shouldRespondWith` 503

    describe "GET /snapshot/utxo" $ do
      responseChannel <- runIO newTChanIO
      prop "responds correctly" $ \nodeState -> do
        let utxo :: Maybe (UTxOType SimpleTx) = getSnapshotUtxo (headState nodeState)
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure nodeState)
              cantCommit
              getPendingDeposits
              putClientInput
              300
              responseChannel
          )
          $ do
            get "/snapshot/utxo"
              `shouldRespondWith` case utxo of
                Nothing -> 404
                Just u -> 200{matchBody = matchJSON u}
      responseChannelSimpleTx <- runIO newTChanIO
      prop "ok response matches schema" $ \nodeState -> do
        let mUTxO = getSnapshotUtxo (headState nodeState)
            utxo :: UTxOType Tx = fromMaybe mempty mUTxO
        withMaxSuccess 4
          . cover 1 (UTxO.null utxo) "empty"
          . cover 1 (not $ UTxO.null utxo) "non empty"
          . withJsonSpecifications
          $ \schemaDir -> do
            withApplication
              ( httpApp @Tx
                  nullTracer
                  dummyChainHandle
                  testEnvironment
                  dummyStatePath
                  defaultPParams
                  (pure nodeState)
                  cantCommit
                  getPendingDeposits
                  putClientInput
                  300
                  responseChannelSimpleTx
              )
              $ do
                get "/snapshot/utxo"
                  `shouldRespondWith` case mUTxO of
                    Nothing -> 404
                    Just _ ->
                      200
                        { matchBody =
                            matchValidJSON
                              (schemaDir </> "api.json")
                              (key "channels" . key "/snapshot/utxo" . key "subscribe" . key "message" . key "payload")
                        }

      prop "has inlineDatumRaw" $ \(i, closedState) ->
        forAll genTxOut $ \o -> do
          let o' = modifyTxOutDatum (const $ mkTxOutDatumInline (123 :: Integer)) o
          let utxo' :: UTxOType Tx = UTxO.fromList [(i, o')]
              ClosedState{confirmedSnapshot} = closedState
              confirmedSnapshot' =
                case confirmedSnapshot of
                  InitialSnapshot{headId} -> InitialSnapshot{headId, initialUTxO = utxo'}
                  ConfirmedSnapshot{snapshot, signatures} ->
                    let Snapshot{headId, version, number, confirmed, utxoToCommit, utxoToDecommit} = snapshot
                        snapshot' = Snapshot{headId, version, number, confirmed, utxo = utxo', utxoToCommit, utxoToDecommit}
                     in ConfirmedSnapshot{snapshot = snapshot', signatures}
              closedState' = closedState{confirmedSnapshot = confirmedSnapshot'}
          withApplication
            ( httpApp @Tx
                nullTracer
                dummyChainHandle
                testEnvironment
                dummyStatePath
                defaultPParams
                (pure NodeInSync{headState = Closed closedState', pendingDeposits = mempty, currentSlot = ChainSlot 0})
                cantCommit
                getPendingDeposits
                putClientInput
                300
                responseChannelSimpleTx
            )
            $ do
              get "/snapshot/utxo"
                `shouldRespondWith` 200
                  { matchBody = MatchBody $ \_ body ->
                      if isNothing (body ^? key (fromString $ Text.unpack $ renderTxIn i) . key "inlineDatumRaw")
                        then Just $ "\ninlineDatumRaw not found in body:\n" <> show body
                        else Nothing
                  }

    describe "POST /commit" $ do
      let getHeadId = pure $ NormalCommit (generateWith arbitrary 42)
      let workingChainHandle =
            dummyChainHandle
              { draftCommitTx = \_ _ -> do
                  tx <- generate $ arbitrary @Tx
                  pure $ Right tx
              }
      let initialHeadState = Initial (generateWith arbitrary 42)
      let openHeadState = Open (generateWith arbitrary 42)
      responseChannel <- runIO newTChanIO
      prop "responds on valid requests" $ \(request :: DraftCommitTxRequest Tx) ->
        withApplication
          ( httpApp
              nullTracer
              workingChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure NodeInSync{headState = initialHeadState, pendingDeposits = mempty, currentSlot = ChainSlot 0})
              getHeadId
              getPendingDeposits
              putClientInput
              300
              responseChannel
          )
          $ do
            post "/commit" (Aeson.encode request)
              `shouldRespondWith` 200

      let failingChainHandle :: PostTxError tx -> Chain tx IO
          failingChainHandle postTxError =
            dummyUnsafeChainHandle
              { draftCommitTx = \_ _ -> pure $ Left postTxError
              , draftDepositTx = \_ _ _ _ _ -> pure $ Left postTxError
              }

      prop "reject deposits with less than min ADA" $ do
        forAll (genUTxOAdaOnlyOfSize 1) $ \(utxo :: UTxO) -> do
          let result = rejectLowDeposits pparams utxo
          case result of
            Left DepositTooLow{providedValue, minimumValue} ->
              property $
                minimumValue >= providedValue
                  & counterexample ("Minimum value: " <> show minimumValue <> " Provided value: " <> show providedValue)
            _ -> property True

      prop "handles PostTxErrors accordingly" $ \request postTxError -> do
        let coverage = case postTxError of
              CommittedTooMuchADAForMainnet{} -> cover 1 True "CommittedTooMuchADAForMainnet"
              UnsupportedLegacyOutput{} -> cover 1 True "UnsupportedLegacyOutput"
              InvalidHeadId{} -> cover 1 True "InvalidHeadId"
              CannotFindOwnInitial{} -> cover 1 True "CannotFindOwnInitial"
              DepositTooLow{} -> cover 1 True "DepositTooLow"
              AmountTooLow{} -> cover 1 True "AmountTooLow"
              FailedToConstructDepositTx{} -> cover 1 True "FailedToConstructDepositTx"
              FailedToDraftTxNotInitializing -> cover 1 True "FailedToDraftTxNotInitializing"
              _ -> property
        checkCoverage
          $ coverage
          $ withApplication
            ( httpApp @Tx
                nullTracer
                (failingChainHandle postTxError)
                testEnvironment
                dummyStatePath
                defaultPParams
                (pure NodeInSync{headState = openHeadState, pendingDeposits = mempty, currentSlot = ChainSlot 0})
                getHeadId
                getPendingDeposits
                putClientInput
                300
                responseChannel
            )
          $ do
            post "/commit" (Aeson.encode (request :: DraftCommitTxRequest Tx))
              `shouldRespondWith` case postTxError of
                CommittedTooMuchADAForMainnet{} -> 400
                UnsupportedLegacyOutput{} -> 400
                CannotFindOwnInitial{} -> 400
                DepositTooLow{} -> 400
                AmountTooLow{} -> 400
                FailedToConstructDepositTx{} -> 400
                FailedToDraftTxNotInitializing -> 500{matchBody = fromString "{\"tag\":\"FailedToDraftTxNotInitializing\"}"}
                _ -> 500

      it "gives information on when the Head was initialized" $
        withTempDir "http-server-spec" $ \tmpDir -> do
          let stateLines =
                [ "{\"eventId\":163,\"stateChanged\":{\"chainSlot\":232,\"tag\":\"TickObserved\"},\"time\":\"2025-10-08T09:33:02.224666984Z\"}"
                , "{\"eventId\":164,\"stateChanged\":{\"chainSlot\":235,\"tag\":\"HeadInitialized\"},\"time\":\"2025-10-08T09:33:02.30814188Z\"}"
                , "{\"eventId\":258,\"stateChanged\":{\"chainSlot\":232,\"tag\":\"TickObserved\"},\"time\":\"2025-10-08T09:33:02.224666984Z\"}"
                , "{\"eventId\":258,\"stateChanged\":{\"chainSlot\":232,\"tag\":\"TickObserved\"},\"time\":\"2025-10-08T09:33:02.224666984Z\"}"
                , "{\"eventId\":300,\"stateChanged\":{\"chainSlot\":300,\"tag\":\"HeadInitialized\"},\"time\":\"2025-10-08T10:33:02.30814188Z\"}"
                ]
          let statePath = tmpDir </> "state"
          writeFileText statePath (unlines stateLines)

          withApplication
            ( httpApp @Tx
                nullTracer
                dummyChainHandle
                testEnvironment
                statePath
                defaultPParams
                (pure NodeInSync{headState = initialHeadState, pendingDeposits = mempty, currentSlot = ChainSlot 152})
                getHeadId
                getPendingDeposits
                putClientInput
                300
                responseChannel
            )
            $ get "/head-initialization" `shouldRespondWith` 200{matchBody = fromString "\"2025-10-08T10:33:02.30814188Z\""}

    describe "POST /transaction" $ do
      let mkReq :: SimpleTx -> LBS.ByteString
          mkReq tx = encode $ SubmitL2TxRequest tx
          testTx = SimpleTx 42 mempty mempty
          testHeadId = generateWith arbitrary 42
      now <- runIO getCurrentTime

      prop "returns 202 Accepted on timeout" $ do
        responseChannel <- newTChanIO
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ pure ())
              0
              responseChannel
          )
          $ do
            post "/transaction" (mkReq testTx) `shouldRespondWith` 202

      prop "returns 200 OK on confirmed snapshot" $ do
        responseChannel <- newTChanIO
        let snapshot =
              Snapshot
                { headId = testHeadId
                , version = 1
                , number = 7
                , confirmed = [testTx]
                , utxo = mempty
                , utxoToCommit = mempty
                , utxoToDecommit = mempty
                }
            event =
              TimedServerOutput
                { output = SnapshotConfirmed{snapshot = snapshot, signatures = mempty, headId = testHeadId}
                , seq = 0
                , time = now
                }
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Left event))
              10
              responseChannel
          )
          $ do
            post "/transaction" (mkReq testTx) `shouldRespondWith` 200

      prop "returns 400 Bad Request on invalid tx" $ do
        responseChannel <- newTChanIO
        let validationError = ValidationError "some error"
            event =
              TimedServerOutput
                { output =
                    TxInvalid
                      { headId = testHeadId
                      , utxo = mempty
                      , transaction = testTx
                      , validationError = validationError
                      }
                , seq = 0
                , time = now
                }
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Left event))
              10
              responseChannel
          )
          $ do
            post "/transaction" (mkReq testTx) `shouldRespondWith` 400

      prop "returns 503 on RejectedInput" $ do
        responseChannel <- newTChanIO
        let clientFailed = RejectedInput{clientInput = NewTx testTx, reason = "chain out of sync"}
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inUnsyncedIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Right clientFailed))
              10
              responseChannel
          )
          $ do
            post "/transaction" (mkReq testTx) `shouldRespondWith` 503

    describe "POST /decommit" $ do
      it "returns 202 on timeout" $ do
        responseChannel <- newTChanIO
        let tx = SimpleTx 1 mempty mempty
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ pure ())
              0
              responseChannel
          )
          $ do
            post "/decommit" (encode tx) `shouldRespondWith` 202

      it "returns 200 on DecommitFinalized" $ do
        responseChannel <- newTChanIO
        let tx = SimpleTx 1 mempty mempty
        now' <- getCurrentTime
        let event =
              TimedServerOutput
                { output = DecommitFinalized{headId = generateWith arbitrary 42, distributedUTxO = mempty}
                , seq = 0
                , time = now'
                }
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Left event))
              10
              responseChannel
          )
          $ do
            post "/decommit" (encode tx) `shouldRespondWith` 200

      it "returns 400 on DecommitInvalid or CommandFailed" $ do
        responseChannel <- newTChanIO
        let tx = SimpleTx 1 mempty mempty
        now' <- getCurrentTime
        let invalid =
              TimedServerOutput
                { output = DecommitInvalid{headId = generateWith arbitrary 42, decommitTx = tx, decommitInvalidReason = DecommitAlreadyInFlight (txId tx)}
                , seq = 0
                , time = now'
                }
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Left invalid))
              10
              responseChannel
          )
          $ do
            post "/decommit" (encode tx) `shouldRespondWith` 400

      it "returns 503 on RejectedInput" $ do
        responseChannel <- newTChanIO
        let tx = SimpleTx 1 mempty mempty
        let clientFailed = RejectedInput{clientInput = Decommit tx, reason = "chain out of sync"}
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inUnsyncedIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Right clientFailed))
              10
              responseChannel
          )
          $ do
            post "/decommit" (encode tx) `shouldRespondWith` 503

    describe "DELETE /commits/:txid full unencoded TxId" $ do
      it "returns 202 on timeout" $ do
        responseChannel <- newTChanIO
        withApplication
          ( httpApp @Tx
              nullTracer
              dummyChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure $ error "Not called")
              (pure CannotCommit)
              (pure [])
              (const $ pure ())
              0
              responseChannel
          )
          $ do
            -- endpoint path formats the txid as JSON in the path; spec helper keeps the path
            delete "/commits/d2c03a20bce36bf86888827f642e69d259ed12c8322a71a9089b057ea81a25ac" `shouldRespondWith` 202

    describe "DELETE /commits/:txid" $ do
      it "returns 202 on timeout" $ do
        responseChannel <- newTChanIO
        let txidJson = LBS.toStrict (encode (txId (SimpleTx 1 mempty mempty)))
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ pure ())
              0
              responseChannel
          )
          $ do
            -- endpoint path formats the txid as JSON in the path; spec helper keeps the path
            delete ("/commits/" <> txidJson) `shouldRespondWith` 202

      it "returns 200 on CommitRecovered" $ do
        responseChannel <- newTChanIO
        now' <- getCurrentTime
        let event =
              TimedServerOutput
                { output = CommitRecovered{headId = generateWith arbitrary 42, recoveredUTxO = mempty, recoveredTxId = txId (SimpleTx 1 mempty mempty)}
                , seq = 0
                , time = now'
                }
        let txidText = LBS.toStrict (encode (txId (SimpleTx 1 mempty mempty)))
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Left event))
              10
              responseChannel
          )
          $ do
            delete ("/commits/" <> txidText) `shouldRespondWith` 200

      it "returns 503 on RejectedInput" $ do
        responseChannel <- newTChanIO
        let tx = SimpleTx 1 mempty mempty
        let txidText = LBS.toStrict (encode (txId tx))
        let clientFailed = RejectedInput{clientInput = Recover (txId tx), reason = "chain out of sync"}
        withApplication
          ( httpApp @SimpleTx
              nullTracer
              dummySimpleChainHandle
              testEnvironment
              dummyStatePath
              defaultPParams
              (pure inUnsyncedIdleState)
              (pure CannotCommit)
              (pure [])
              (const $ atomically $ writeTChan responseChannel (Right clientFailed))
              10
              responseChannel
          )
          $ do
            delete ("/commits/" <> txidText) `shouldRespondWith` 503

-- * Helpers

-- | Create a 'ResponseMatcher' or 'MatchBody' from a JSON serializable value
-- (using their 'IsString' instances).
matchJSON :: (IsString s, ToJSON a) => a -> s
matchJSON = fromString . decodeUtf8 . encode

-- | Create a 'MatchBody' that validates the returned JSON response against a
-- schema. NOTE: This raises impure exceptions, so only use it in this test
-- suite.
matchValidJSON :: FilePath -> SchemaSelector -> MatchBody
matchValidJSON schemaFile selector =
  MatchBody $ \_headers body ->
    case eitherDecode body of
      Left err -> Just $ "failed to decode body: " <> err
      Right value -> validateJSONPure value
 where
  -- NOTE: Uses unsafePerformIO to create a pure API although we are actually
  -- calling an external program to verify the schema. This is fine, because the
  -- call is referentially transparent and any given invocation of schema file,
  -- selector and value will always yield the same result and can be shared.
  validateJSONPure value =
    unsafePerformIO $ do
      validateJSON schemaFile selector value
      pure Nothing
