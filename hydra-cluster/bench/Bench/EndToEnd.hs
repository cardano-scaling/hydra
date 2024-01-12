{-# LANGUAGE DuplicateRecordFields #-}

module Bench.EndToEnd where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.Summary (Summary (..), makeQuantiles)
import CardanoClient (RunningNode (..), awaitTransaction, submitTransaction, submitTx)
import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTVarIO),
  check,
  lengthTBQueue,
  modifyTVar,
  newTBQueueIO,
  newTVarIO,
  tryReadTBQueue,
  writeTBQueue,
 )
import Control.Lens (to, (^?))
import Control.Monad.Class.MonadAsync (mapConcurrently)
import Data.Aeson (Result (Error, Success), Value, encode, fromJSON, (.=))
import Data.Aeson.Lens (key, _Array, _JSON, _Number, _String)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Time (UTCTime (UTCTime), utctDayTime)
import Hydra.Cardano.Api (Tx, TxId, UTxO, getVerificationKey, serialiseToTextEnvelope, signTx)
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Scenarios (
  EndToEndLog (..),
  headIsInitializingWith,
 )
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Crypto (generateSigningKey)
import Hydra.Generator (ClientDataset (..), ClientKeys (..), Dataset (..))
import Hydra.Ledger (txId)
import Hydra.Logging (Tracer, withTracerOutputTo)
import Hydra.Party (deriveParty)
import HydraNode (
  HydraClient,
  hydraNodeId,
  input,
  output,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
 )
import System.Directory (findExecutable)
import System.FilePath ((</>))
import System.IO (hGetLine, hPutStrLn)
import System.Process (
  CreateProcess (..),
  StdStream (CreatePipe),
  proc,
  withCreateProcess,
 )
import Text.Printf (printf)
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Prelude (read)

data Event = Event
  { submittedAt :: UTCTime
  , validAt :: Maybe UTCTime
  , invalidAt :: Maybe UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

bench :: Int -> NominalDiffTime -> FilePath -> Dataset -> IO Summary
bench startingNodeId timeoutSeconds workDir dataset@Dataset{clientDatasets, title, description} = do
  putStrLn $ "Test logs available in: " <> (workDir </> "test.log")
  withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
    withTracerOutputTo hdl "Test" $ \tracer ->
      failAfter timeoutSeconds $ do
        putTextLn "Starting benchmark"
        let cardanoKeys = map (\ClientDataset{clientKeys = ClientKeys{signingKey}} -> (getVerificationKey signingKey, signingKey)) clientDatasets
        let hydraKeys = generateSigningKey . show <$> [1 .. toInteger (length cardanoKeys)]
        let parties = Set.fromList (deriveParty <$> hydraKeys)
        let clusterSize = fromIntegral $ length clientDatasets
        withOSStats workDir $
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \node@RunningNode{nodeSocket} -> do
            putTextLn "Seeding network"
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- seedNetwork node dataset (contramap FromFaucet tracer)
            let contestationPeriod = UnsafeContestationPeriod 10
            putStrLn $ "Starting hydra cluster in " <> workDir
            withHydraCluster hydraTracer workDir nodeSocket startingNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \(leader :| followers) -> do
              let clients = leader : followers
              waitForNodesConnected hydraTracer 20 clients

              putTextLn "Initializing Head"
              send leader $ input "Init" []
              headId <-
                waitForAllMatch (fromIntegral $ 10 * clusterSize) clients $
                  headIsInitializingWith parties

              putTextLn "Comitting initialUTxO from dataset"
              expectedUTxO <- commitUTxO node clients dataset

              waitFor hydraTracer (fromIntegral $ 10 * clusterSize) clients $
                output "HeadIsOpen" ["utxo" .= expectedUTxO, "headId" .= headId]

              putTextLn "HeadIsOpen"
              processedTransactions <- processTransactions clients dataset

              putTextLn "Closing the Head"
              send leader $ input "Close" []

              deadline <- waitMatch 300 leader $ \v -> do
                guard $ v ^? key "tag" == Just "HeadIsClosed"
                guard $ v ^? key "headId" == Just (toJSON headId)
                v ^? key "contestationDeadline" . _JSON

              -- Expect to see ReadyToFanout within 3 seconds after deadline
              remainingTime <- diffUTCTime deadline <$> getCurrentTime
              waitFor hydraTracer (remainingTime + 3) [leader] $
                output "ReadyToFanout" ["headId" .= headId]

              putTextLn "Finalizing the Head"
              send leader $ input "Fanout" []
              waitMatch 100 leader $ \v -> do
                guard (v ^? key "tag" == Just "HeadIsFinalized")
                guard $ v ^? key "headId" == Just (toJSON headId)

              let res = mapMaybe analyze . Map.toList $ processedTransactions
                  aggregates = movingAverage res

              writeResultsCsv (workDir </> "results.csv") aggregates

              let confTimes = map (\(_, _, a) -> a) res
                  numberOfTxs = length confTimes
                  numberOfInvalidTxs = length $ Map.filter (isJust . invalidAt) processedTransactions
                  averageConfirmationTime = sum confTimes / fromIntegral numberOfTxs
                  quantiles = makeQuantiles confTimes
                  summaryTitle = fromMaybe "Baseline Scenario" title
                  summaryDescription = fromMaybe defaultDescription description

              pure $ Summary{clusterSize, numberOfTxs, averageConfirmationTime, quantiles, summaryTitle, summaryDescription, numberOfInvalidTxs}

defaultDescription :: Text
defaultDescription = ""

-- | Collect OS-level stats while running some 'IO' action.
--
-- __NOTE__: This function relies on [dstat](https://linux.die.net/man/1/dstat). If the executable is not in the @PATH@
-- it's basically a no-op.
--
-- Writes a @system.csv@ file into given `workDir` containing one line every 5 second with share of user CPU load.
-- Here is a sample content:
--
-- @@
-- 2022-02-16 14:25:43.67203351 UTC,11
-- 2022-02-16 14:25:48.669817664 UTC,10
-- 2022-02-16 14:25:53.672050421 UTC,14
-- 2022-02-16 14:25:58.670460796 UTC,12
-- 2022-02-16 14:26:03.669831775 UTC,11
-- 2022-02-16 14:26:08.67203726 UTC,10
-- ...
-- @@
--
-- TODO: add more data points for memory and network consumption
withOSStats :: FilePath -> IO a -> IO a
withOSStats workDir action =
  findExecutable "dstat" >>= \case
    Nothing -> action
    Just exePath ->
      withCreateProcess (process exePath){std_out = CreatePipe} $ \_stdin out _stderr _processHandle ->
        race
          (collectStats out $ workDir </> "system.csv")
          action
          >>= \case
            Left () -> failure "dstat process failed unexpectedly"
            Right a -> pure a
 where
  process exePath = (proc exePath ["-cm", "-n", "-N", "lo", "--integer", "--noheaders", "--noupdate", "5"]){cwd = Just workDir}

  collectStats Nothing _ = pure ()
  collectStats (Just hdl) filepath =
    withFile filepath WriteMode $ \file ->
      forever $
        hGetLine hdl >>= processStat file

  processStat :: Handle -> String -> IO ()
  processStat file stat =
    case getAllTextMatches (stat =~ ("[0-9]+" :: String)) :: [String] of
      (cpu : _) -> do
        now <- getCurrentTime
        hPutStrLn file $ show now <> "," <> show ((read cpu :: Double) / 100)
      _ -> pure ()

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

      fiveSecSlice (timeSlice -> t1, _, _) (timeSlice -> t2, _, _) = t1 == t2

      fst3 (a, _, _) = a
      snd3 (_, a, _) = a
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
-- dataset, and also publish the hydra scripts. The 'TxId' of the publishing
-- transaction is returned.
seedNetwork :: RunningNode -> Dataset -> Tracer IO FaucetLog -> IO TxId
seedNetwork node@RunningNode{nodeSocket, networkId} Dataset{fundingTransaction, clientDatasets} tracer = do
  fundClients
  forM_ clientDatasets fuelWith100Ada
  putTextLn "Publishing hydra scripts"
  publishHydraScriptsAs node Faucet
 where
  fundClients = do
    putTextLn "Fund scenario from faucet"
    submitTransaction networkId nodeSocket fundingTransaction
    void $ awaitTransaction networkId nodeSocket fundingTransaction

  fuelWith100Ada ClientDataset{clientKeys = ClientKeys{signingKey}} = do
    let vk = getVerificationKey signingKey
    putTextLn $ "Seed client " <> show vk
    seedFromFaucet node vk 100_000_000 tracer

-- | Commit all (expected to exit) 'initialUTxO' from the dataset using the
-- (asumed same sequence) of clients.
commitUTxO :: RunningNode -> [HydraClient] -> Dataset -> IO UTxO
commitUTxO node clients Dataset{clientDatasets} =
  mconcat <$> forM (zip clients clientDatasets) doCommit
 where
  doCommit (client, ClientDataset{initialUTxO, clientKeys = ClientKeys{externalSigningKey}}) = do
    requestCommitTx client initialUTxO
      <&> signTx externalSigningKey
        >>= submitTx node
    pure initialUTxO

processTransactions :: [HydraClient] -> Dataset -> IO (Map.Map TxId Event)
processTransactions clients Dataset{clientDatasets} = do
  let processors = zip (zip clientDatasets (cycle clients)) [1 ..]
  mconcat <$> mapConcurrently (uncurry clientProcessDataset) processors
 where
  clientProcessDataset (ClientDataset{txSequence}, client) clientId = do
    let numberOfTxs = length txSequence
    submissionQ <- newTBQueueIO (fromIntegral numberOfTxs)
    registry <- newRegistry
    atomically $ forM_ txSequence $ writeTBQueue submissionQ
    submitTxs client registry submissionQ
      `concurrently_` waitForAllConfirmations client registry (Set.fromList $ map txId txSequence)
      `concurrently_` progressReport (hydraNodeId client) clientId numberOfTxs submissionQ
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

--
-- Helpers
--

assignUTxO :: (UTxO, Int) -> Map.Map Int (HydraClient, UTxO) -> Map.Map Int (HydraClient, UTxO)
assignUTxO (utxo, clientId) = Map.adjust appendUTxO clientId
 where
  appendUTxO (client, utxo') = (client, utxo <> utxo')

noUTxOs :: UTxO
noUTxOs = mempty

double :: Real a => a -> Double
double = realToFrac

int :: Int -> Int
int = id

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
  send client $ input "NewTx" ["transaction" .= serialiseToTextEnvelope Nothing tx]

data WaitResult
  = TxInvalid {transactionId :: TxId, reason :: Text}
  | TxValid {transactionId :: TxId}
  | SnapshotConfirmed {txIds :: [Value], snapshotNumber :: Scientific}

data Registry tx = Registry
  { processedTxs :: TVar IO (Map.Map TxId Event)
  , latestSnapshot :: TVar IO Scientific
  }

newRegistry ::
  IO (Registry Tx)
newRegistry = do
  processedTxs <- newTVarIO mempty
  latestSnapshot <- newTVarIO 0
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

  maybeTxValid v = do
    guard (v ^? key "tag" == Just "TxValid")
    v
      ^? key "transaction" . key "id" . to fromJSON >>= \case
        Error _ -> Nothing
        Success txid -> pure $ TxValid txid

  maybeTxInvalid v = do
    guard (v ^? key "tag" == Just "TxInvalid")
    v
      ^? key "transaction" . key "id" . to fromJSON >>= \case
        Error _ -> Nothing
        Success tx ->
          TxInvalid tx <$> v ^? key "validationError" . key "reason" . _String

  maybeSnapshotConfirmed v = do
    guard (v ^? key "tag" == Just "SnapshotConfirmed")
    snapshot <- v ^? key "snapshot"
    SnapshotConfirmed
      <$> snapshot
        ^? key "confirmedTransactions"
          . _Array
          . to toList
      <*> snapshot
        ^? key "snapshotNumber"
          . _Number

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

  toCsv (a, b, c, d) = show a <> "," <> encode b <> "," <> encode c <> "," <> encode d <> "\n"
