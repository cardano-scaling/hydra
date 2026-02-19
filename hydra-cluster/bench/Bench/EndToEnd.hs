{-# LANGUAGE DuplicateRecordFields #-}

module Bench.EndToEnd where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.Summary (Summary (..), SystemStats, makeQuantiles)
import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (findRunningCardanoNode', withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTVarIO),
  check,
  lengthTBQueue,
  modifyTVar,
  tryReadTBQueue,
  writeTBQueue,
 )
import Control.Lens (to, (^..), (^?))
import Control.Monad.Class.MonadAsync (mapConcurrently)
import Data.Aeson (Result (Error, Success), Value, encode, fromJSON, (.=))
import Data.Aeson.Lens (key, values, _JSON, _Number, _String)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Time (UTCTime (UTCTime), utctDayTime)
import Hydra.Cardano.Api (Era, NetworkId, SocketPath, Tx, TxId, UTxO, getVerificationKey, lovelaceToValue, signTx)
import Hydra.Chain.Backend (ChainBackend)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Cluster.Faucet (FaucetLog (..), publishHydraScriptsAs, returnFundsToFaucet', seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Scenarios (EndToEndLog (..))
import Hydra.Generator (ClientDataset (..), Dataset (..))
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
  HydraNodeLog,
  hydraNodeId,
  input,
  output,
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
import Test.HUnit.Lang (formatFailureReason)
import Text.Printf (printf)

bench :: Int -> NominalDiffTime -> FilePath -> Dataset -> IO (Summary, SystemStats)
bench startingNodeId timeoutSeconds workDir dataset = do
  putStrLn $ "Test logs available in: " <> (workDir </> "test.log")
  withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
    withTracerOutputTo (BlockBuffering (Just 64000)) hdl "Test" $ \tracer ->
      failAfter timeoutSeconds $ do
        putTextLn "Starting benchmark"
        let cardanoKeys = hydraNodeKeys dataset <&> \sk -> (getVerificationKey sk, sk)
        let hydraKeys = generateSigningKey . show <$> [1 .. toInteger (length cardanoKeys)]
        statsTvar <- newLabelledTVarIO "bench-stats" mempty
        scenarioData <- withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \blockTime backend -> do
          let nodeSocket' = case Backend.getOptions backend of
                Direct DirectOptions{nodeSocket} -> nodeSocket
                _ -> error "Unexpected Blockfrost backend"
          putTextLn "Seeding network"
          seedNetwork backend dataset (contramap FromFaucet tracer)
          putTextLn "Publishing hydra scripts"
          hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
          putStrLn $ "Starting hydra cluster in " <> workDir
          let hydraTracer = contramap FromHydraNode tracer
          withHydraCluster hydraTracer blockTime workDir nodeSocket' startingNodeId cardanoKeys hydraKeys hydraScriptsTxId 10 $ \clients -> do
            waitForNodesConnected hydraTracer 20 clients
            scenario hydraTracer backend workDir dataset clients
        systemStats <- readTVarIO statsTvar
        pure (scenarioData, systemStats)

benchDemo ::
  NetworkId ->
  SocketPath ->
  NominalDiffTime ->
  [Host] ->
  FilePath ->
  Dataset ->
  IO (Summary, SystemStats)
benchDemo networkId nodeSocket timeoutSeconds hydraClients workDir dataset@Dataset{clientDatasets} = do
  putStrLn $ "Test logs available in: " <> (workDir </> "test.log")
  withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
    withTracerOutputTo (BlockBuffering (Just 64000)) hdl "Test" $ \tracer ->
      failAfter timeoutSeconds $ do
        putTextLn "Starting benchmark demo"
        let cardanoTracer = contramap FromCardanoNode tracer
        findRunningCardanoNode' cardanoTracer networkId nodeSocket >>= \case
          Nothing ->
            error ("Not found running node at socket: " <> show nodeSocket <> ", and network: " <> show networkId)
          Just (_blockTime, backend) -> do
            putTextLn "Seeding network"
            seedNetwork backend dataset (contramap FromFaucet tracer)
            (`finally` returnFaucetFunds tracer backend) $ do
              putStrLn $ "Connecting to hydra cluster: " <> show hydraClients
              let hydraTracer = contramap FromHydraNode tracer
              withHydraClientConnections hydraTracer (hydraClients `zip` [1 ..]) [] $ \case
                [] -> error "no hydra clients provided"
                (leader : followers) ->
                  (,[]) <$> scenario hydraTracer backend workDir dataset (leader :| followers)
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
        withConnectionToNodeHost tracer peerId apiHost (Just "/?history=no") $ \con -> do
          withHydraClientConnections tracer rest (con : connections) action

  returnFaucetFunds tracer backend = do
    putTextLn "Returning funds to faucet"
    let faucetTracer = contramap FromFaucet tracer
    forM (hydraNodeKeys dataset <> (paymentKey <$> clientDatasets)) $ \sk -> do
      returnAmount <- returnFundsToFaucet' faucetTracer backend sk
      traceWith faucetTracer $ ReturnedFunds{returnAmount}

-- | Runs the benchmark scenario given a list of clients. The first client is
-- used to drive the life-cycle of the head.
scenario ::
  ChainBackend backend =>
  Tracer IO HydraNodeLog ->
  backend ->
  FilePath ->
  Dataset ->
  NonEmpty HydraClient ->
  IO Summary
scenario hydraTracer backend workDir Dataset{clientDatasets, title, description} nonEmptyClients = do
  let clusterSize = fromIntegral $ length clientDatasets
  let leader = head nonEmptyClients
      clients = toList nonEmptyClients
  let totalTxs = sum $ map (length . txSequence) clientDatasets

  putTextLn "Initializing Head"
  send leader $ input "Init" []
  headId :: HeadId <-
    waitForAllMatch (fromIntegral $ 10 * clusterSize) clients $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsInitializing"
      v ^? key "headId" . _JSON

  putTextLn "Committing initialUTxO from dataset"
  expectedUTxO <- commitUTxO backend clients clientDatasets

  waitFor hydraTracer (fromIntegral $ 10 * clusterSize) clients $
    output "HeadIsOpen" ["utxo" .= expectedUTxO, "headId" .= headId]

  putTextLn "HeadIsOpen"
  processedTransactions <- processTransactions clients clientDatasets

  putTextLn "Closing the Head"
  send leader $ input "Close" []

  deadline <- waitMatch 300 leader $ \v -> do
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
  finalUTxOJSON <- waitMatch 100 leader $ \v -> do
    guard (v ^? key "tag" == Just "HeadIsFinalized")
    guard $ v ^? key "headId" == Just (toJSON headId)
    v ^? key "utxo"

  let confTimes = map (\(_, _, a) -> a) res
      numberOfTxs = length confTimes
      numberOfInvalidTxs = length $ Map.filter (isJust . invalidAt) processedTransactions
      averageConfirmationTime = sum confTimes / fromIntegral numberOfTxs
      quantiles = makeQuantiles confTimes
      summaryTitle = fromMaybe "Baseline Scenario" title
      summaryDescription = fromMaybe defaultDescription description
      numberOfFanoutOutputs =
        case parseEither (parseJSON @(UTxO.UTxO Era)) finalUTxOJSON of
          Left _ -> error "Failed to decode Fanout UTxO"
          Right fanoutUTxO -> UTxO.size fanoutUTxO

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
      }

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
-- dataset.
seedNetwork :: ChainBackend backend => backend -> Dataset -> Tracer IO FaucetLog -> IO ()
seedNetwork backend Dataset{fundingTransaction, hydraNodeKeys} tracer = do
  fundClients hydraNodeKeys
  forM_ hydraNodeKeys fuelWith100Ada
 where
  fundClients hydraSKeys = do
    putTextLn "Fund scenario from faucet"
    Backend.submitTransaction backend fundingTransaction
    let vks = getVerificationKey <$> hydraSKeys
    mapM_ (Backend.awaitTransaction backend fundingTransaction) vks

  fuelWith100Ada signingKey = do
    let vk = getVerificationKey signingKey
    putTextLn $ "Fuel node key " <> show vk
    seedFromFaucet backend vk (lovelaceToValue 100_000_000) tracer

-- | Commit all (expected to exit) 'initialUTxO' from the dataset using the
-- (assumed same sequence) of clients.
commitUTxO :: ChainBackend backend => backend -> [HydraClient] -> [ClientDataset] -> IO UTxO
commitUTxO backend clients clientDatasets =
  mconcat <$> forM (zip clients clientDatasets) doCommit
 where
  doCommit (client, ClientDataset{initialUTxO, paymentKey}) = do
    requestCommitTx client initialUTxO
      <&> signTx paymentKey
        >>= Backend.submitTransaction backend
    pure initialUTxO

data Event = Event
  { submittedAt :: UTCTime
  , validAt :: Maybe UTCTime
  , invalidAt :: Maybe UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Eq, Show)

processTransactions :: [HydraClient] -> [ClientDataset] -> IO (Map.Map TxId Event)
processTransactions clients clientDatasets = do
  let processors = zip (zip clientDatasets (cycle clients)) [1 ..]
  mconcat <$> mapConcurrently (uncurry clientProcessDataset) processors
 where
  formatLocation = maybe "" (\loc -> "at " <> prettySrcLoc loc)

  clientProcessDataset (ClientDataset{txSequence}, client) clientId = do
    let numberOfTxs = length txSequence
    submissionQ <- newLabelledTBQueueIO "submission" (fromIntegral numberOfTxs)
    registry <- newRegistry
    atomically $ forM_ txSequence $ writeTBQueue submissionQ
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
    readTVarIO (processedTxs registry)

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
  , latestSnapshot :: TVar IO Scientific
  }

newRegistry ::
  IO (Registry Tx)
newRegistry = do
  processedTxs <- newLabelledTVarIO "registry-processed-txs" mempty
  latestSnapshot <- newLabelledTVarIO "registry-latest-snapshot" 0
  pure $ Registry{processedTxs, latestSnapshot}

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
waitForAllConfirmations n1 Registry{processedTxs} allIds = do
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
          SnapshotConfirmed{txIds} -> do
            confirmedIds <- mapM (confirmTx processedTxs) txIds
            go $ remainingIds \\ Set.fromList confirmedIds

  waitForSnapshotConfirmation = waitMatch 20 n1 $ \v ->
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

writeResultsCsv :: FilePath -> [(UTCTime, NominalDiffTime, NominalDiffTime, Int)] -> IO ()
writeResultsCsv fp res = do
  putStrLn $ "Writing results to: " <> fp
  writeFileLBS fp $ headers <> "\n" <> foldMap toCsv res
 where
  headers = "txId,confirmationTime"

  toCsv :: (UTCTime, NominalDiffTime, NominalDiffTime, Int) -> LBS.ByteString
  toCsv (a, b, c, d) = show a <> "," <> encode b <> "," <> encode c <> "," <> encode d <> "\n"
