{-# LANGUAGE DuplicateRecordFields #-}

module Bench.EndToEnd where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.Summary (Summary (..), SystemStats, makeDoubleQuantiles, makeQuantiles)
import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (EndToEndLog (..), HydraNodeLog, findRunningCardanoNode', runBackend, withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTVarIO),
  check,
  lengthTBQueue,
  modifyTVar,
  tryReadTBQueue,
  writeTBQueue,
 )
import Control.Lens (to, (^..), (^?))
import Control.Monad.Class.MonadAsync (concurrently, mapConcurrently)
import Data.Aeson (Result (Error, Success), Value, encode, fromJSON, (.=))
import Data.Aeson.Lens (key, values, _JSON, _Number, _String)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Time (UTCTime (UTCTime), utctDayTime)
import Data.Vector (Vector)
import Hydra.Cardano.Api (NetworkId, PaymentKey, SigningKey, SocketPath, Tx, TxId, UTxO, getVerificationKey, lovelaceToValue, signTx, txOutAddress, txOutValue)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Cluster.Faucet (FaucetLog (..), publishHydraScriptsAs, returnFundsToFaucet', seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (Timing (..), depositTimeout)
import Hydra.Generator (ClientDataset (..), Dataset (..))
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Logging (
  Tracer,
  traceWith,
  withTracerOutputTo,
 )
import Hydra.Network (Host)
import Hydra.Options (ChainBackendOptions (..), DirectOptions (..))
import Hydra.Tx (HeadId, txId)
import Hydra.Tx.Crypto (generateSigningKey)
import HydraNode (
  HydraClient,
  apiHost,
  getSnapshotUTxO,
  hydraNodeId,
  input,
  output,
  postDecommit,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withConnectionToNodeHost,
  withHydraCluster,
 )
import System.FilePath ((</>))
import System.Process.Typed (shell, withProcessTerm)
import Test.HUnit.Lang (formatFailureReason)
import Text.Printf (printf)

bench :: Int -> NominalDiffTime -> Bool -> FilePath -> Dataset -> IO (Summary, SystemStats)
bench startingNodeId timeoutSeconds incrementalOpsEnabled workDir dataset = do
  putStrLn $ "Test logs available in: " <> (workDir </> "test.log")
  withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
    withTracerOutputTo (BlockBuffering (Just 64000)) hdl "Test" $ \tracer ->
      failAfter timeoutSeconds $ do
        putTextLn "Starting benchmark"
        let cardanoKeys = hydraNodeKeys dataset <&> \sk -> (getVerificationKey sk, sk)
        let hydraKeys = generateSigningKey . show <$> [1 .. toInteger (length cardanoKeys)]
        statsTvar <- newLabelledTVarIO "bench-stats" mempty
        scenarioData <- withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \blockTime directOpts -> do
          let opts = Direct directOpts
          let contestationPeriod = truncate $ 10 * blockTime
          let DirectOptions{nodeSocket = nodeSocket'} = directOpts
          putTextLn "Seeding network"
          sideUTxOs <- seedNetwork opts dataset incrementalOpsEnabled (contramap FromFaucet tracer)
          putTextLn "Publishing hydra scripts"
          hydraScriptsTxId <- publishHydraScriptsAs opts Faucet
          putStrLn $ "Starting hydra cluster in " <> workDir
          let hydraTracer = contramap FromHydraNode tracer
          let timing = Timing{blockTime, contestationPeriod, depositPeriod = truncate $ 50 * blockTime}
          putStrLn $ "Timing: " <> show timing
          withHydraCluster hydraTracer timing workDir nodeSocket' startingNodeId cardanoKeys hydraKeys hydraScriptsTxId $ \clients -> do
            waitForNodesConnected hydraTracer 20 clients
            scenario hydraTracer timing opts workDir dataset clients Nothing sideUTxOs
        systemStats <- readTVarIO statsTvar
        pure (scenarioData, systemStats)

benchDemo ::
  NetworkId ->
  SocketPath ->
  NominalDiffTime ->
  [Host] ->
  Maybe String ->
  Bool ->
  FilePath ->
  Dataset ->
  IO (Summary, SystemStats)
benchDemo networkId nodeSocket timeoutSeconds hydraClients pumbaCommand incrementalOpsEnabled workDir dataset@Dataset{clientDatasets} = do
  putStrLn $ "Test logs available in: " <> (workDir </> "test.log")
  withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
    withTracerOutputTo (BlockBuffering (Just 64000)) hdl "Test" $ \tracer ->
      failAfter timeoutSeconds $ do
        putTextLn "Starting benchmark demo"
        let cardanoTracer = contramap FromCardanoNode tracer
        findRunningCardanoNode' cardanoTracer networkId nodeSocket >>= \case
          Nothing ->
            error ("Not found running node at socket: " <> show nodeSocket <> ", and network: " <> show networkId)
          Just (blockTime, directOpts) -> do
            let opts = Direct directOpts
            putTextLn "Seeding network"
            sideUTxOs <- seedNetwork opts dataset incrementalOpsEnabled (contramap FromFaucet tracer)
            (`finally` returnFaucetFunds tracer opts) $ do
              putStrLn $ "Connecting to hydra cluster: " <> show hydraClients
              let hydraTracer = contramap FromHydraNode tracer
              -- XXX: Assumes contestation and deposit periods
              let timing = Timing{blockTime, contestationPeriod = truncate $ 10 * blockTime, depositPeriod = truncate $ 20 * blockTime}
              withHydraClientConnections hydraTracer (hydraClients `zip` [1 ..]) [] $ \case
                [] -> error "no hydra clients provided"
                (leader : followers) ->
                  (,[]) <$> scenario hydraTracer timing opts workDir dataset (leader :| followers) pumbaCommand sideUTxOs
 where
  withHydraClientConnections ::
    Tracer IO HydraNodeLog ->
    [(Host, Int)] ->
    [HydraClient] ->
    ([HydraClient] -> IO a) ->
    IO a
  withHydraClientConnections tracer apiHosts connections action = do
    case apiHosts of
      [] -> action connections
      ((apiHost, peerId) : rest) -> do
        withConnectionToNodeHost tracer peerId apiHost Nothing (Just "/?history=no") $ \con -> do
          withHydraClientConnections tracer rest (con : connections) action

  returnFaucetFunds tracer opts = do
    putTextLn "Returning funds to faucet"
    let faucetTracer = contramap FromFaucet tracer
    forM (hydraNodeKeys dataset <> (paymentKey <$> clientDatasets)) $ \sk -> do
      returnAmount <- returnFundsToFaucet' faucetTracer opts sk
      traceWith faucetTracer $ ReturnedFunds{returnAmount}

-- | Runs the benchmark scenario given a list of clients. The first client is
-- used to drive the life-cycle of the head.
scenario ::
  Tracer IO HydraNodeLog ->
  Timing ->
  ChainBackendOptions ->
  FilePath ->
  Dataset ->
  NonEmpty HydraClient ->
  Maybe String ->
  -- | Optional pre-seeded side UTxOs, one per client dataset, used for
  -- interleaved incremental commit/decommit cycles. Empty list disables.
  [UTxO] ->
  IO Summary
scenario hydraTracer timing opts workDir Dataset{clientDatasets, title, description} nonEmptyClients pumbaCommand sideUTxOs = do
  let clusterSize = fromIntegral $ length clientDatasets
  let leader = head nonEmptyClients
      clients = toList nonEmptyClients
  let totalTxs = sum $ map (length . txSequence) clientDatasets

  putTextLn "Initializing Head"
  send leader $ input "Init" []
  headId :: HeadId <-
    waitForAllMatch (5 * blockTime) clients $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsOpen"
      v ^? key "headId" . _JSON

  putTextLn "Depositing initialUTxO from datasets"
  depositTxs <- commitUTxO opts clients clientDatasets

  putTextLn $ "Waiting for deposits to finalize: " <> show (txId <$> depositTxs)
  -- NOTE: Need to wait for any CommitFinalized and only assert ids after as
  -- waitForAllMatch skips over messages otherwise.
  deposits <- replicateM (length depositTxs) $
    waitForAllMatch (depositTimeout timing * fromIntegral clusterSize * 30) clients $ \v -> do
      guard $ v ^? key "tag" == Just "CommitFinalized"
      guard $ v ^? key "headId" == Just (toJSON headId)
      v ^? key "depositTxId" >>= parseMaybe parseJSON
  Set.fromList deposits `shouldBe` Set.fromList (txId <$> depositTxs)

  putTextLn "HeadIsOpen with deposits finalized"

  -- Note: We only run Pumba during normal transaction processing; this is
  -- acceptable because otherwise we do not retry the particular actions that
  -- may or may not be dropped.
  let incrementalCtx =
        if null sideUTxOs
          then Nothing
          else Just IncrementalContext{ctxBackend = opts, ctxSideUTxOs = sideUTxOs, ctxHeadId = headId, ctxTracer = hydraTracer}
  (processedTransactions, snapshotsSeen, incrementalCommitTimes, incrementalDecommitTimes) <-
    withPumba pumbaCommand $ processTransactions clients clientDatasets incrementalCtx

  putTextLn "Closing the Head"
  send leader $ input "Close" []

  deadline <- waitMatch (20 * blockTime) leader $ \v -> do
    guard $ v ^? key "tag" == Just "HeadIsClosed"
    guard $ v ^? key "headId" == Just (toJSON headId)
    v ^? key "contestationDeadline" . _JSON

  -- Write the results already in case we cannot finalize
  let res = mapMaybe analyze . Map.toList $ processedTransactions
      aggregates = movingAverage res

  writeResultsCsv (workDir </> "results.csv") aggregates

  -- Expect to see ReadyToFanout within 3 seconds after deadline
  remainingTime <- diffUTCTime deadline <$> getCurrentTime
  waitFor hydraTracer (remainingTime + 3) [leader] $
    output "ReadyToFanout" ["headId" .= headId]

  putTextLn "Finalizing the Head"
  send leader $ input "Fanout" []
  fanoutResult :: Either SomeException Int <- try $ waitMatch 100 leader $ \v -> do
    guard (v ^? key "tag" == Just "HeadIsFinalized")
    guard $ v ^? key "headId" == Just (toJSON headId)
    finalizedArr <- v ^? key "finalizedUTxO"
    pure $ length (finalizedArr ^.. values)

  numberOfFanoutOutputs <-
    case fanoutResult of
      Left _ -> do
        putStrLn "Fanout failed."
        UTxO.size <$> getSnapshotUTxO leader
      Right n -> pure n

  let confTimes = map (\(_, _, a) -> a) res
      numberOfTxs = length confTimes
      numberOfInvalidTxs = length $ Map.filter (isJust . invalidAt) processedTransactions
      averageConfirmationTime = sum confTimes / fromIntegral numberOfTxs
      quantiles = makeQuantiles confTimes
      summaryTitle = fromMaybe "Baseline Scenario" title
      summaryDescription = fromMaybe defaultDescription description
      (endToEndTps, snapshotTpsQuantiles, numberOfSnapshots) =
        computeThroughput processedTransactions snapshotsSeen

  pure $
    Summary
      { clusterSize
      , totalTxs
      , numberOfTxs
      , averageConfirmationTime
      , quantiles
      , summaryTitle
      , summaryDescription
      , numberOfInvalidTxs
      , numberOfFanoutOutputs
      , endToEndTps
      , snapshotTpsQuantiles
      , numberOfSnapshots
      , incrementalCommitTimes
      , incrementalDecommitTimes
      }
 where
  Timing{blockTime} = timing

  withPumba :: Maybe String -> IO a -> IO a
  withPumba Nothing action = action
  withPumba (Just cmd) action = do
    putTextLn $ "Starting pumba: " <> toText cmd
    withProcessTerm (shell cmd) $ const action

defaultDescription :: Text
defaultDescription = ""

-- | Compute average confirmation/validation time over intervals of 5 seconds.
--
-- Given a stream of (possibly unordered) data points for validation and confirmation time,
-- this function will order and group them in 5s intervals, and compute the average of
-- timings for this interval. It also outputs the /count/ of values for each interval.
--
-- __NOTE__: The timestamp of the grouped values is set to the beginning of the 5s time
-- slice of the group.
movingAverage :: [(UTCTime, NominalDiffTime, NominalDiffTime)] -> [(UTCTime, NominalDiffTime, NominalDiffTime, Int)]
movingAverage confirmations =
  let window :: Num a => a
      window = 5

      fiveSeconds = List.groupBy fiveSecSlice $ sortOn fst3 confirmations

      timeSlice t@UTCTime{utctDayTime} =
        t{utctDayTime = fromIntegral (floor (utctDayTime / window) * window :: Integer)}

      fiveSecSlice :: (UTCTime, NominalDiffTime, NominalDiffTime) -> (UTCTime, NominalDiffTime, NominalDiffTime) -> Bool
      fiveSecSlice (timeSlice -> t1, _, _) (timeSlice -> t2, _, _) = t1 == t2

      fst3 :: (a, b, c) -> a
      fst3 (a, _, _) = a
      snd3 :: (a, b, c) -> b
      snd3 (_, a, _) = a
      thd3 :: (a, b, c) -> c
      thd3 (_, _, a) = a

      average = \case
        [] -> error "empty group"
        slice@((t, _, _) : _) ->
          let n = length slice
           in ( timeSlice t
              , sum (map snd3 slice) / fromIntegral n
              , sum (map thd3 slice) / fromIntegral n
              , n `div` window
              )
   in map average fiveSeconds

-- | Distribute 100 ADA fuel, starting funds from faucet for each client in the
-- dataset. When 'incrementalOpsEnabled' is True, additionally seeds a small
-- side UTxO (10 ADA) per client used later for interleaved incremental
-- commit/decommit cycles. The returned list is parallel to
-- 'clientDatasets' (empty when the flag is off).
seedNetwork :: ChainBackendOptions -> Dataset -> Bool -> Tracer IO FaucetLog -> IO [UTxO]
seedNetwork opts Dataset{fundingTransaction, hydraNodeKeys, clientDatasets} incrementalOpsEnabled tracer = do
  fundClients hydraNodeKeys
  forM_ hydraNodeKeys fuelWith100Ada
  if incrementalOpsEnabled
    then do
      putTextLn "Funding side UTxOs for incremental ops"
      forM clientDatasets $ \ClientDataset{paymentKey} ->
        seedFromFaucet opts (getVerificationKey paymentKey) (lovelaceToValue 10_000_000) tracer
    else pure []
 where
  fundClients hydraSKeys = do
    putTextLn "Fund scenario from faucet"
    runBackend opts $ submitTransaction fundingTransaction
    let vks = getVerificationKey <$> hydraSKeys
    forM_ vks $ \vk -> runBackend opts (awaitTransaction fundingTransaction vk)

  fuelWith100Ada signingKey = do
    let vk = getVerificationKey signingKey
    putTextLn $ "Fuel node key " <> show vk
    seedFromFaucet opts vk (lovelaceToValue 100_000_000) tracer

-- | Deposit all 'initialUTxO' of each client data set.
commitUTxO :: ChainBackendOptions -> [HydraClient] -> [ClientDataset] -> IO [Tx]
commitUTxO opts clients clientDatasets =
  forM (zip clients clientDatasets) doCommit
 where
  doCommit (client, ClientDataset{initialUTxO, paymentKey}) = do
    depositTx <-
      requestCommitTx client initialUTxO
        <&> signTx paymentKey
    runBackend opts $ submitTransaction depositTx
    pure depositTx

data Event = Event
  { submittedAt :: UTCTime
  , validAt :: Maybe UTCTime
  , invalidAt :: Maybe UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Eq, Show)

data IncrementalContext = IncrementalContext
  { ctxBackend :: ChainBackendOptions
  , ctxSideUTxOs :: [UTxO]
  , ctxHeadId :: HeadId
  , ctxTracer :: Tracer IO HydraNodeLog
  }

processTransactions ::
  [HydraClient] ->
  [ClientDataset] ->
  Maybe IncrementalContext ->
  IO (Map.Map TxId Event, Map.Map Scientific (UTCTime, Int), [NominalDiffTime], [NominalDiffTime])
processTransactions clients clientDatasets incrementalCtx = do
  -- Allocate per-client state up front so the optional incremental ops thread
  -- can share access to the per-client registries.
  perClient <- forM (zip clientDatasets (cycle clients)) $ \(cd@ClientDataset{txSequence}, client) -> do
    let n = length txSequence
    submissionQ <- newLabelledTBQueueIO "submission" (fromIntegral n)
    registry <- newRegistry
    atomically $ forM_ txSequence $ writeTBQueue submissionQ
    pure (cd, client, submissionQ, registry, n)

  let runIncremental = case incrementalCtx of
        Nothing -> pure ([], [])
        Just ctx -> runAllIncrementalOps ctx perClient

  let perClientActions =
        zipWith
          ( \clientId (cd, client, submissionQ, registry, n) ->
              clientProcessDataset cd client clientId submissionQ registry n
          )
          [1 ..]
          perClient

  (clientResults, (commitTimes, decommitTimes)) <-
    concurrently
      (mapConcurrently identity perClientActions)
      runIncremental

  let mergedTxs = Map.unions (map fst clientResults)
      earlierObservation :: (UTCTime, Int) -> (UTCTime, Int) -> (UTCTime, Int)
      earlierObservation (t1, c) (t2, _) = (min t1 t2, c)
      mergedSnapshots = foldr (Map.unionWith earlierObservation . snd) Map.empty clientResults
  pure (mergedTxs, mergedSnapshots, commitTimes, decommitTimes)
 where
  formatLocation = maybe "" (\loc -> "at " <> prettySrcLoc loc)

  clientProcessDataset ClientDataset{txSequence} client clientId submissionQ registry numberOfTxs = do
    concurrentlyLabelled_
      ("submit-txs", submitTxs client registry submissionQ)
      ( "confirm-txs"
      , concurrentlyLabelled_
          ("wait-for-all-confirmations", waitForAllConfirmations client registry (Set.fromList $ map txId txSequence))
          ("progress-report", progressReport (hydraNodeId client) clientId numberOfTxs submissionQ)
      )
      `catch` \(HUnitFailure sourceLocation reason) ->
        putStrLn ("Something went wrong while waiting for all confirmations: " <> formatLocation sourceLocation <> ": " <> formatFailureReason reason)
          `catch` \(ex :: SomeException) ->
            putStrLn ("Something went wrong while waiting for all confirmations: " <> show ex)
    (,) <$> readTVarIO (processedTxs registry) <*> readTVarIO (observedSnapshots registry)

  runAllIncrementalOps :: IncrementalContext -> [(ClientDataset, HydraClient, TBQueue IO Tx, Registry Tx, Int)] -> IO ([NominalDiffTime], [NominalDiffTime])
  runAllIncrementalOps IncrementalContext{ctxBackend, ctxSideUTxOs, ctxTracer} perClient = do
    let pairs = zip ctxSideUTxOs perClient
    results <-
      mapConcurrently
        ( \(sideUTxO, (ClientDataset{paymentKey}, client, submissionQ, _registry, numberOfTxs)) ->
            try @_ @SomeException
              (runOneIncrementalOp ctxTracer ctxBackend client paymentKey sideUTxO numberOfTxs submissionQ)
        )
        pairs
    let collected = rights results
        commits = mapMaybe fst collected
        decommits = mapMaybe snd collected
    forM_ results $ \case
      Left ex -> putStrLn $ "Incremental op failed: " <> show ex
      _ -> pure ()
    pure (commits, decommits)

  -- Opens a fresh API connection so the wait on CommitFinalized/DecommitFinalized
  -- doesn't compete with the existing waitForAllConfirmations on the same WS.
  runOneIncrementalOp ::
    Tracer IO HydraNodeLog ->
    ChainBackendOptions ->
    HydraClient ->
    SigningKey PaymentKey ->
    UTxO ->
    Int ->
    TBQueue IO Tx ->
    IO (Maybe NominalDiffTime, Maybe NominalDiffTime)
  runOneIncrementalOp tracer backend client paymentKey sideUTxO numberOfTxs submissionQ = do
    -- Wait until ~half this client's queue has drained.
    atomically $ do
      remaining <- lengthTBQueue submissionQ
      let drained = fromIntegral numberOfTxs - fromIntegral remaining :: Double
          target = fromIntegral numberOfTxs / 2 :: Double
      check (drained >= target)

    withConnectionToNodeHost tracer (hydraNodeId client) (apiHost client) Nothing (Just "/?history=no") $ \obs -> do
      putTextLn "Incremental: requesting commit"
      startCommit <- getCurrentTime
      depositTx <- requestCommitTx client sideUTxO <&> signTx paymentKey
      let depositTxId = txId depositTx
      runBackend backend $ submitTransaction depositTx
      _ <- waitMatch 180 obs $ \v -> do
        guard (v ^? key "tag" == Just "CommitFinalized")
        observed <- v ^? key "depositTxId" >>= parseMaybe parseJSON
        guard (observed == depositTxId)
        pure ()
      commitFinalisedAt <- getCurrentTime
      let commitTime = commitFinalisedAt `diffUTCTime` startCommit
      putTextLn $ "Incremental: commit finalised in " <> show commitTime

      case UTxO.toList sideUTxO of
        [] -> pure (Just commitTime, Nothing)
        ((i, o) : _) ->
          case mkSimpleTx (i, o) (txOutAddress o, txOutValue o) paymentKey of
            Left err -> do
              putStrLn $ "Incremental: decommit tx build failed: " <> show err
              pure (Just commitTime, Nothing)
            Right decommitTx -> do
              putTextLn "Incremental: posting decommit"
              startDecommit <- getCurrentTime
              postDecommit client decommitTx
              let decommitTxId = txId decommitTx
              _ <- waitMatch 180 obs $ \v -> do
                guard (v ^? key "tag" == Just "DecommitFinalized")
                observed <- v ^? key "decommitTxId" >>= parseMaybe parseJSON
                guard (observed == decommitTxId)
                pure ()
              decommitFinalisedAt <- getCurrentTime
              let decommitTime = decommitFinalisedAt `diffUTCTime` startDecommit
              putTextLn $ "Incremental: decommit finalised in " <> show decommitTime
              pure (Just commitTime, Just decommitTime)

progressReport :: Int -> Int -> Int -> TBQueue IO Tx -> IO ()
progressReport nodeId clientId queueSize queue = do
  len <- atomically (lengthTBQueue queue)
  if len == (0 :: Natural)
    then pure ()
    else do
      let progress :: Double = (1 - fromIntegral len / fromIntegral queueSize) * 100.0
      putStrLn $ printf "Client %d (node %d): %d/%d (%.02f%%)" clientId nodeId (queueSize - fromIntegral len) queueSize progress
      threadDelay 5
      progressReport nodeId clientId queueSize queue

type TransactionId = Integer
type TransactionInput = Int
type TransactionOutput = Int

newTx ::
  TVar IO (Map.Map TxId Event) ->
  HydraClient ->
  Tx ->
  IO ()
newTx registry client tx = do
  now <- getCurrentTime
  atomically $
    modifyTVar registry $
      Map.insert (txId tx) $
        Event
          { submittedAt = now
          , validAt = Nothing
          , invalidAt = Nothing
          , confirmedAt = Nothing
          }
  send client $ input "NewTx" ["transaction" .= toJSON tx]

data WaitResult
  = TxInvalid {transactionId :: TxId, reason :: Text}
  | TxValid {transactionId :: TxId}
  | SnapshotConfirmed {txIds :: [Value], number :: Scientific}

data Registry tx = Registry
  { processedTxs :: TVar IO (Map.Map TxId Event)
  , observedSnapshots :: TVar IO (Map.Map Scientific (UTCTime, Int))
  }

newRegistry ::
  IO (Registry Tx)
newRegistry = do
  processedTxs <- newLabelledTVarIO "registry-processed-txs" mempty
  observedSnapshots <- newLabelledTVarIO "registry-observed-snapshots" mempty
  pure $ Registry{processedTxs, observedSnapshots}

submitTxs ::
  HydraClient ->
  Registry Tx ->
  TBQueue IO Tx ->
  IO ()
submitTxs client registry@Registry{processedTxs} submissionQ = do
  txToSubmit <- atomically $ tryReadTBQueue submissionQ
  case txToSubmit of
    Just tx -> do
      newTx processedTxs client tx
      waitTxIsConfirmed (txId tx)
      submitTxs client registry submissionQ
    Nothing -> pure ()
 where
  waitTxIsConfirmed txid =
    atomically $ do
      event <- Map.lookup txid <$> readTVar processedTxs
      check (isJust $ confirmedAt =<< event)

waitForAllConfirmations ::
  HydraClient ->
  Registry Tx ->
  Set TxId ->
  IO ()
waitForAllConfirmations n1 Registry{processedTxs, observedSnapshots} allIds = do
  go allIds
 where
  go remainingIds
    | Set.null remainingIds = do
        putStrLn "All transactions confirmed. Sweet!"
    | otherwise = do
        waitForSnapshotConfirmation >>= \case
          TxValid{transactionId} -> do
            validTx processedTxs transactionId
            go remainingIds
          TxInvalid{transactionId} -> do
            invalidTx processedTxs transactionId
            go $ Set.delete transactionId remainingIds
          SnapshotConfirmed{txIds, number} -> do
            now <- getCurrentTime
            atomically $
              modifyTVar observedSnapshots $
                Map.insertWith (\_new old -> old) number (now, length txIds)
            confirmedIds <- mapM (confirmTx processedTxs) txIds
            go $ remainingIds \\ Set.fromList confirmedIds

  -- 60s (was 20s) so the pumba network-loss benchmark
  -- ('.github/workflows/network-test.yaml', up to 90% packet loss) has
  -- enough headroom for snapshot confirmation under repeated gRPC
  -- retries. Tighten only after re-running that workflow.
  waitForSnapshotConfirmation = waitMatch 60 n1 $ \v ->
    maybeTxValid v <|> maybeTxInvalid v <|> maybeSnapshotConfirmed v

  maybeTxValid :: Value -> Maybe WaitResult
  maybeTxValid v = do
    guard (v ^? key "tag" == Just "TxValid")
    v
      ^? key "transactionId"
        . to fromJSON
        >>= \case
          Error _ -> Nothing
          Success txid -> pure $ TxValid txid

  maybeTxInvalid :: Value -> Maybe WaitResult
  maybeTxInvalid v = do
    guard (v ^? key "tag" == Just "TxInvalid")
    v
      ^? key "transaction"
        . key "txId"
        . to fromJSON
        >>= \case
          Error _ -> Nothing
          Success tx ->
            TxInvalid tx <$> v ^? key "validationError" . key "reason" . _String

  maybeSnapshotConfirmed :: Value -> Maybe WaitResult
  maybeSnapshotConfirmed v = do
    guard (v ^? key "tag" == Just "SnapshotConfirmed")
    snapshot <- v ^? key "snapshot"
    number <- snapshot ^? key "number" . _Number
    pure $
      SnapshotConfirmed
        { txIds = snapshot ^.. key "confirmed" . values . key "txId"
        , number
        }

confirmTx ::
  TVar IO (Map.Map TxId Event) ->
  Value ->
  IO TxId
confirmTx registry tx = do
  case fromJSON @TxId tx of
    Success identifier -> do
      now <- getCurrentTime
      atomically $
        modifyTVar registry $
          Map.adjust (\e -> e{confirmedAt = Just now}) identifier
      pure identifier
    _ -> error $ "incorrect Txid" <> show tx

validTx ::
  TVar IO (Map.Map TxId Event) ->
  TxId ->
  IO ()
validTx registry txid = do
  now <- getCurrentTime
  atomically $
    modifyTVar registry $
      Map.adjust (\e -> e{validAt = Just now}) txid

invalidTx ::
  TVar IO (Map.Map TxId Event) ->
  TxId ->
  IO ()
invalidTx registry txid = do
  now <- getCurrentTime
  atomically $
    modifyTVar registry $
      Map.adjust (\e -> e{invalidAt = Just now}) txid

analyze :: (TxId, Event) -> Maybe (UTCTime, NominalDiffTime, NominalDiffTime)
analyze = \case
  (_, Event{submittedAt, validAt = Just valid, confirmedAt = Just conf}) ->
    Just (submittedAt, valid `diffUTCTime` submittedAt, conf `diffUTCTime` submittedAt)
  _ -> Nothing

-- | End-to-end TPS over the run plus quantiles of per-snapshot TPS.
--
-- End-to-end TPS divides the count of confirmed txs by the wall-clock window
-- from earliest submission to latest confirmation. Per-snapshot TPS is
-- derived for snapshots that confirmed at least one tx by dividing the tx
-- count by the gap to the previous observation (or the earliest submission
-- time for the first snapshot).
computeThroughput :: Map.Map TxId Event -> Map.Map Scientific (UTCTime, Int) -> (Double, Vector Double, Int)
computeThroughput txs snapshots =
  let submitted = mapMaybe submittedAtFor (Map.elems txs)
      confirmed = mapMaybe confirmedAt (Map.elems txs)
      numberOfTxs = length confirmed
      e2eTps = case (submitted, confirmed) of
        (s : _, _ : _) ->
          let span_ = realToFrac (List.maximum confirmed `diffUTCTime` List.minimum (s : submitted)) :: Double
           in if span_ > 0 then fromIntegral numberOfTxs / span_ else 0
        _ -> 0
      sortedSnapshots = sortOn fst (Map.toList snapshots)
      anchor = case submitted of
        [] -> Nothing
        _ -> Just (List.minimum submitted)
      perSnapshotRates = perSnapshotTps anchor sortedSnapshots
   in (e2eTps, makeDoubleQuantiles perSnapshotRates, length sortedSnapshots)
 where
  submittedAtFor Event{submittedAt} = Just submittedAt
  perSnapshotTps :: Maybe UTCTime -> [(Scientific, (UTCTime, Int))] -> [Double]
  perSnapshotTps = go
   where
    go :: Maybe UTCTime -> [(Scientific, (UTCTime, Int))] -> [Double]
    go _ [] = []
    go prev ((_, (t, c)) : rest)
      | c <= 0 = go (Just t) rest
      | otherwise =
          case prev of
            Just p ->
              let dt = realToFrac (t `diffUTCTime` p) :: Double
                  rate = if dt > 0 then fromIntegral c / dt else 0
               in rate : go (Just t) rest
            Nothing -> go (Just t) rest

writeResultsCsv :: FilePath -> [(UTCTime, NominalDiffTime, NominalDiffTime, Int)] -> IO ()
writeResultsCsv fp res = do
  putStrLn $ "Writing results to: " <> fp
  writeFileLBS fp $ headers <> "\n" <> foldMap toCsv res
 where
  headers = "txId,confirmationTime"

  toCsv :: (UTCTime, NominalDiffTime, NominalDiffTime, Int) -> LBS.ByteString
  toCsv (a, b, c, d) = show a <> "," <> encode b <> "," <> encode c <> "," <> encode d <> "\n"
