{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Bench.EndToEnd where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Control.Lens (to, (^?))
import Control.Monad.Class.MonadAsync (mapConcurrently)
import Control.Monad.Class.MonadSTM (
  MonadSTM (readTVarIO),
  check,
  lengthTBQueue,
  modifyTVar,
  newTBQueueIO,
  newTVarIO,
  tryReadTBQueue,
  writeTBQueue,
 )
import Data.Aeson (Result (Error, Success), Value, encode, fromJSON, (.=))
import Data.Aeson.Lens (key, _Array, _JSON, _Number, _String)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Time (UTCTime (UTCTime), nominalDiffTimeToSeconds, utctDayTime)
import Hydra.Cardano.Api (Tx, TxId, UTxO, getVerificationKey)
import Hydra.Chain.CardanoClient (awaitTransaction, submitTransaction)
import Hydra.Cluster.Faucet (Marked (Fuel), publishHydraScriptsAs, seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (Faucet), defaultNetworkId)
import Hydra.Crypto (generateSigningKey)
import Hydra.Generator (ClientDataset (..), Dataset (..))
import Hydra.Ledger (txId)
import Hydra.Logging (withTracerOutputTo)
import Hydra.Party (deriveParty)
import HydraNode (
  EndToEndLog (FromCardanoNode),
  HydraClient,
  hydraNodeId,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withNewClient,
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
  , confirmedAt :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON)

bench :: DiffTime -> FilePath -> Dataset -> Word64 -> Spec
bench timeoutSeconds workDir dataset@Dataset{clientDatasets} clusterSize =
  specify ("Load test on " <> show clusterSize <> " local nodes in " <> workDir) $ do
    withFile (workDir </> "test.log") ReadWriteMode $ \hdl ->
      withTracerOutputTo hdl "Test" $ \tracer ->
        failAfter timeoutSeconds $ do
          putTextLn "Starting benchmark"
          let cardanoKeys = map (\ClientDataset{signingKey} -> (getVerificationKey signingKey, signingKey)) clientDatasets
          let hydraKeys = generateSigningKey . show <$> [1 .. toInteger (length cardanoKeys)]
          let parties = Set.fromList (deriveParty <$> hydraKeys)
          withOSStats workDir $
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) workDir $ \node@RunningNode{nodeSocket} -> do
              putTextLn "Seeding network"
              hydraScriptsTxId <- seedNetwork node dataset
              withHydraCluster tracer workDir nodeSocket 0 cardanoKeys hydraKeys hydraScriptsTxId $ \(leader :| followers) -> do
                let clients = leader : followers
                waitForNodesConnected tracer clients

                putTextLn "Initializing Head"
                let contestationPeriod = 10 :: Natural
                send leader $ input "Init" ["contestationPeriod" .= contestationPeriod]
                waitFor tracer (fromIntegral $ 10 * clusterSize) clients $
                  output "ReadyToCommit" ["parties" .= parties]

                putTextLn "Comitting initialUTxO from dataset"
                expectedUTxO <- commitUTxO clients dataset

                waitFor tracer (fromIntegral $ 10 * clusterSize) clients $
                  output "HeadIsOpen" ["utxo" .= expectedUTxO]

                putTextLn "HeadIsOpen"
                processedTransactions <- processTransactions clients dataset

                putTextLn "Closing the Head"
                send leader $ input "Close" []
                deadline <- waitMatch 3 leader $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsClosed"
                  v ^? key "contestationDeadline" . _JSON

                -- Expect to see ReadyToFanout within 3 seconds after deadline
                remainingTime <- diffUTCTime deadline <$> getCurrentTime
                waitFor tracer (truncate $ remainingTime + 3) [leader] $
                  output "ReadyToFanout" []

                putTextLn "Finalizing the Head"
                send leader $ input "Fanout" []
                waitMatch 10 leader $ \v ->
                  guard (v ^? key "tag" == Just "HeadIsFinalized")

                let res = mapMaybe analyze . Map.toList $ processedTransactions
                    aggregates = movingAverage res

                writeResultsCsv (workDir </> "results.csv") aggregates

                -- TODO: Create a proper summary
                let confTimes = map (\(_, _, a) -> a) res
                    below1Sec = filter (< 1) confTimes
                    avgConfirmation = double (nominalDiffTimeToSeconds $ sum confTimes) / double (length confTimes)
                    percentBelow1Sec = double (length below1Sec) / double (length confTimes) * 100
                putTextLn $ "Confirmed txs: " <> show (length confTimes)
                putTextLn $ "Average confirmation time: " <> show avgConfirmation
                putTextLn $ "Confirmed below 1 sec: " <> show percentBelow1Sec <> "%"
                percentBelow1Sec `shouldSatisfy` (> 90)

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
withOSStats :: FilePath -> IO () -> IO ()
withOSStats workDir action =
  findExecutable "dstat" >>= \case
    Nothing -> action
    Just exePath ->
      withCreateProcess (process exePath){std_out = CreatePipe} $ \_stdin out _stderr _processHandle ->
        race_
          (collectStats out $ workDir </> "system.csv")
          action
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
seedNetwork :: RunningNode -> Dataset -> IO TxId
seedNetwork node@RunningNode{nodeSocket} Dataset{fundingTransaction, clientDatasets} = do
  fundClients
  forM_ clientDatasets fuelWith100Ada
  publishHydraScriptsAs node Faucet
 where
  fundClients = do
    submitTransaction defaultNetworkId nodeSocket fundingTransaction
    void $ awaitTransaction defaultNetworkId nodeSocket fundingTransaction

  fuelWith100Ada ClientDataset{signingKey} = do
    let vk = getVerificationKey signingKey
    seedFromFaucet node vk 100_000_000 Fuel

-- | Commit all (expected to exit) 'initialUTxO' from the dataset using the
-- (asumed same sequence) of clients.
commitUTxO :: [HydraClient] -> Dataset -> IO UTxO
commitUTxO clients Dataset{clientDatasets} =
  mconcat <$> forM (zip clients clientDatasets) doCommit
 where
  doCommit (client, ClientDataset{initialUTxO}) = commit client initialUTxO

processTransactions :: [HydraClient] -> Dataset -> IO (Map.Map TxId Event)
processTransactions clients Dataset{clientDatasets} = do
  let processors = zip (zip clientDatasets (cycle clients)) [1 ..]
  mconcat <$> mapConcurrently (uncurry clientProcessDataset) processors
 where
  clientProcessDataset (ClientDataset{txSequence}, client) clientId = do
    let numberOfTxs = length txSequence
    submissionQ <- newTBQueueIO (fromIntegral numberOfTxs)
    registry <- newRegistry
    withNewClient client $ \client' -> do
      atomically $ forM_ txSequence $ writeTBQueue submissionQ
      submitTxs client' registry submissionQ
        `concurrently_` waitForAllConfirmations client' registry submissionQ (Set.fromList $ map txId txSequence)
        `concurrently_` progressReport (hydraNodeId client') clientId numberOfTxs submissionQ
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

commit :: HydraClient -> UTxO -> IO UTxO
commit client initialUTxO = do
  send client $ input "Commit" ["utxo" .= initialUTxO]
  pure initialUTxO

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
          , confirmedAt = Nothing
          }
  send client $ input "NewTx" ["transaction" .= tx]

data WaitResult
  = TxInvalid {transaction :: Tx, reason :: Text}
  | TxValid {transaction :: Tx}
  | SnapshotConfirmed {transactions :: [Value], snapshotNumber :: Scientific}

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
  TBQueue IO Tx ->
  Set TxId ->
  IO ()
waitForAllConfirmations n1 Registry{processedTxs} submissionQ allIds = do
  go allIds
 where
  go remainingIds
    | Set.null remainingIds = do
      putStrLn "All transactions confirmed. Sweet!"
    | otherwise = do
      waitForSnapshotConfirmation >>= \case
        TxValid{transaction} -> do
          validTx processedTxs (txId transaction)
          go remainingIds
        TxInvalid{transaction} -> do
          atomically $ writeTBQueue submissionQ transaction
          go remainingIds
        SnapshotConfirmed{transactions} -> do
          confirmedIds <- mapM (confirmTx processedTxs) transactions
          go $ remainingIds \\ Set.fromList confirmedIds

  waitForSnapshotConfirmation = waitMatch 20 n1 $ \v ->
    maybeTxValid v <|> maybeTxInvalid v <|> maybeSnapshotConfirmed v

  maybeTxValid v = do
    guard (v ^? key "tag" == Just "TxValid")
    v ^? key "transaction" . to fromJSON >>= \case
      Error _ -> Nothing
      Success tx -> pure $ TxValid tx

  maybeTxInvalid v = do
    guard (v ^? key "tag" == Just "TxInvalid")
    v ^? key "transaction" . to fromJSON >>= \case
      Error _ -> Nothing
      Success tx ->
        TxInvalid tx <$> v ^? key "validationError" . key "reason" . _String

  maybeSnapshotConfirmed v = do
    guard (v ^? key "tag" == Just "SnapshotConfirmed")
    snapshot <- v ^? key "snapshot"
    SnapshotConfirmed
      <$> snapshot ^? key "confirmedTransactions" . _Array . to toList
      <*> snapshot ^? key "snapshotNumber" . _Number

confirmTx ::
  TVar IO (Map.Map TxId Event) ->
  Value ->
  IO TxId
confirmTx registry tx = do
  case fromJSON @TxId <$> tx ^? key "id" of
    Just (Success identifier) -> do
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
