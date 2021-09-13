{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Bench.EndToEnd where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
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
import Data.Aeson.Lens (key, _Array, _Number, _String)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Time (nominalDiffTimeToSeconds)
import Hydra.Ledger (Tx, TxId, Utxo, txId)
import Hydra.Ledger.Cardano (CardanoTx, genFixedSizeSequenceOfValidTransactions, genUtxo)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Party (deriveParty, generateKey)
import HydraNode (
  HydraClient,
  hydraNodeId,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withMockChain,
  withNewClient,
 )
import System.FilePath ((</>))
import Test.QuickCheck (generate)
import Text.Printf (printf)

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

data Dataset = Dataset
  { initialUtxo :: Utxo CardanoTx
  , transactionsSequence :: [CardanoTx]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

generateDataset :: Int -> IO Dataset
generateDataset sequenceLength = do
  initialUtxo <- generate genUtxo
  transactionsSequence <- generate $ genFixedSizeSequenceOfValidTransactions sequenceLength initialUtxo
  pure Dataset{initialUtxo, transactionsSequence}

data Event = Event
  { submittedAt :: UTCTime
  , validAt :: Maybe UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON)

bench :: DiffTime -> FilePath -> [Dataset] -> Word64 -> Spec
bench timeoutSeconds workDir dataset clusterSize =
  specify ("Load test on three local nodes (" <> workDir <> ")") $ do
    showLogsOnFailure $ \tracer ->
      failAfter timeoutSeconds $ do
        withMockChain $ \chainPorts ->
          withHydraCluster tracer workDir chainPorts clusterSize $ \(leader :| followers) -> do
            let nodes = leader : followers
            waitForNodesConnected tracer [1 .. fromIntegral clusterSize] nodes
            let contestationPeriod = 10 :: Natural
            send leader $ input "Init" ["contestationPeriod" .= contestationPeriod]
            let parties = Set.fromList $ map (deriveParty . generateKey) [1 .. fromIntegral clusterSize]
            waitFor tracer 3 nodes $
              output "ReadyToCommit" ["parties" .= parties]

            expectedUtxo <- commit nodes dataset

            waitFor tracer 3 nodes $ output "HeadIsOpen" ["utxo" .= expectedUtxo]

            processedTransactions <- processTransactions nodes dataset

            putTextLn "Closing the Head..."
            send leader $ input "Close" []
            waitMatch (fromIntegral $ 60 * clusterSize) leader $ \v ->
              guard (v ^? key "tag" == Just "HeadIsFinalized")

            let res = mapMaybe analyze . Map.toList $ processedTransactions
            writeResultsCsv (workDir </> "results.csv") res
            -- TODO: Create a proper summary
            let confTimes = map (\(_, _, a) -> a) res
                below1Sec = filter (< 1) confTimes
                avgConfirmation = double (nominalDiffTimeToSeconds $ sum confTimes) / double (length confTimes)
                percentBelow1Sec = double (length below1Sec) / double (length confTimes) * 100
            putTextLn $ "Confirmed txs: " <> show (length confTimes)
            putTextLn $ "Average confirmation time: " <> show avgConfirmation
            putTextLn $ "Confirmed below 1 sec: " <> show percentBelow1Sec <> "%"
            percentBelow1Sec `shouldSatisfy` (> 90)

processTransactions :: [HydraClient] -> [Dataset] -> IO (Map.Map (TxId CardanoTx) Event)
processTransactions clients dataset = do
  let processors = zip (zip dataset (cycle clients)) [1 ..]
  mconcat <$> mapConcurrently (uncurry clientProcessTransactionsSequence) processors
 where
  clientProcessTransactionsSequence (Dataset{transactionsSequence}, client) clientId = do
    let numberOfTxs = length transactionsSequence
    submissionQ <- newTBQueueIO (fromIntegral numberOfTxs)
    registry <- newRegistry
    withNewClient client $ \client' -> do
      atomically $ forM_ transactionsSequence $ writeTBQueue submissionQ
      submitTxs client' registry submissionQ
        `concurrently_` waitForAllConfirmations client' registry submissionQ (Set.fromList $ map txId transactionsSequence)
        `concurrently_` progressReport (hydraNodeId client') clientId numberOfTxs submissionQ
      readTVarIO (processedTxs registry)

progressReport :: Int -> Int -> Int -> TBQueue IO CardanoTx -> IO ()
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

commit :: [HydraClient] -> [Dataset] -> IO (Utxo CardanoTx)
commit clients dataset = do
  let initialMap = Map.fromList $ map (\node -> (hydraNodeId node, (node, noUtxos))) clients
      distributeUtxo = zip (initialUtxo <$> dataset) (cycle $ hydraNodeId <$> clients)
      clientsToUtxo = foldr assignUtxo initialMap distributeUtxo

  forM_ (Map.elems clientsToUtxo) $ \(n, utxo) ->
    send n $ input "Commit" ["utxo" .= utxo]

  pure $ mconcat $ snd <$> Map.elems clientsToUtxo

assignUtxo :: (Utxo CardanoTx, Int) -> Map.Map Int (HydraClient, Utxo CardanoTx) -> Map.Map Int (HydraClient, Utxo CardanoTx)
assignUtxo (utxo, clientId) = Map.adjust appendUtxo clientId
 where
  appendUtxo (client, utxo') = (client, utxo <> utxo')

noUtxos :: Utxo CardanoTx
noUtxos = mempty

double :: Real a => a -> Double
double = realToFrac

int :: Int -> Int
int = id

type TransactionId = Integer
type TransactionInput = Int
type TransactionOutput = Int

newTx ::
  Tx tx =>
  TVar IO (Map.Map (TxId tx) Event) ->
  HydraClient ->
  tx ->
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
  = TxInvalid {transaction :: CardanoTx, reason :: Text}
  | TxValid {transaction :: CardanoTx}
  | SnapshotConfirmed {transactions :: [Value], snapshotNumber :: Scientific}

data Registry tx = Registry
  { processedTxs :: TVar IO (Map.Map (TxId tx) Event)
  , latestSnapshot :: TVar IO Scientific
  }

newRegistry ::
  IO (Registry CardanoTx)
newRegistry = do
  processedTxs <- newTVarIO mempty
  latestSnapshot <- newTVarIO 0
  pure $ Registry{processedTxs, latestSnapshot}

submitTxs ::
  HydraClient ->
  Registry CardanoTx ->
  TBQueue IO CardanoTx ->
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
  Registry CardanoTx ->
  TBQueue IO CardanoTx ->
  Set (TxId CardanoTx) ->
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
  TVar IO (Map.Map (TxId CardanoTx) Event) ->
  Value ->
  IO (TxId CardanoTx)
confirmTx registry tx = do
  case fromJSON @(TxId CardanoTx) <$> tx ^? key "id" of
    Just (Success identifier) -> do
      now <- getCurrentTime
      atomically $
        modifyTVar registry $
          Map.adjust (\e -> e{confirmedAt = Just now}) identifier
      pure identifier
    _ -> error $ "incorrect Txid" <> show tx

validTx ::
  TVar IO (Map.Map (TxId CardanoTx) Event) ->
  TxId CardanoTx ->
  IO ()
validTx registry txid = do
  now <- getCurrentTime
  atomically $
    modifyTVar registry $
      Map.adjust (\e -> e{validAt = Just now}) txid

analyze :: (TxId CardanoTx, Event) -> Maybe (UTCTime, NominalDiffTime, NominalDiffTime)
analyze = \case
  (_, Event{submittedAt, validAt = Just valid, confirmedAt = Just conf}) -> Just (submittedAt, valid `diffUTCTime` submittedAt, conf `diffUTCTime` submittedAt)
  _ -> Nothing

writeResultsCsv :: FilePath -> [(UTCTime, NominalDiffTime, NominalDiffTime)] -> IO ()
writeResultsCsv fp res = do
  putStrLn $ "Writing results to: " <> fp
  writeFileLBS fp $ headers <> "\n" <> foldMap toCsv res
 where
  headers = "txId,confirmationTime"

  toCsv (a, b, c) = show a <> "," <> encode b <> "," <> encode c <> "\n"
