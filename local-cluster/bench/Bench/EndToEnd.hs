{-# LANGUAGE DeriveAnyClass #-}
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
import Control.Concurrent.STM (check)
import Control.Lens (to, (^?))
import Control.Monad.Class.MonadSTM (
  MonadSTM (readTVarIO),
  modifyTVar,
  modifyTVar',
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
import Hydra.Ledger.Cardano (CardanoTx)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (
  HydraClient,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraNode,
  withMockChain,
 )
import System.FilePath ((</>))

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

data Event = Event
  { submittedAt :: UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON)

bench :: FilePath -> Utxo CardanoTx -> [CardanoTx] -> Spec
bench workDir initialUtxo txs =
  specify ("Load test on three local nodes (" <> workDir <> ")") $ do
    registry <- newRegistry txs
    showLogsOnFailure $ \tracer ->
      failAfter 600 $ do
        withMockChain $ \chainPorts ->
          withHydraNode tracer workDir chainPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
            withHydraNode tracer workDir chainPorts 2 bobSk [aliceVk, carolVk] $ \n2 ->
              withHydraNode tracer workDir chainPorts 3 carolSk [aliceVk, bobVk] $ \n3 -> do
                waitForNodesConnected tracer [n1, n2, n3]
                let contestationPeriod = 10 :: Natural
                send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
                waitFor tracer 3 [n1, n2, n3] $
                  output "ReadyToCommit" ["parties" .= [int 10, 20, 30]]

                send n1 $ input "Commit" ["utxo" .= initialUtxo]
                send n2 $ input "Commit" ["utxo" .= noUtxos]
                send n3 $ input "Commit" ["utxo" .= noUtxos]

                waitFor tracer 3 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= initialUtxo]

                submitTxs n1 registry
                  `concurrently_` waitForAllConfirmations n1 registry (Set.fromList . map txId $ txs)

                putTextLn "Closing the Head..."
                send n1 $ input "Close" []
                waitMatch (contestationPeriod + 3) n1 $ \v ->
                  guard (v ^? key "tag" == Just "HeadIsFinalized")

    res <- mapMaybe analyze . Map.toList <$> readTVarIO (confirmedTxs registry)
    writeResultsCsv (workDir </> "results.csv") res
    -- TODO: Create a proper summary
    let confTimes = map snd res
        below1Sec = filter (< 1) confTimes
        avgConfirmation = double (nominalDiffTimeToSeconds $ sum confTimes) / double (length confTimes)
        percentBelow1Sec = double (length below1Sec) / double (length confTimes) * 100
    putTextLn $ "Confirmed txs: " <> show (length confTimes)
    putTextLn $ "Average confirmation time: " <> show avgConfirmation
    putTextLn $ "Confirmed below 1 sec: " <> show percentBelow1Sec <> "%"
    percentBelow1Sec `shouldSatisfy` (> 90)

--
-- Helpers
--

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
          , confirmedAt = Nothing
          }
  send client $ input "NewTx" ["transaction" .= tx]
  putTextLn $ "Submitted tx " <> show (txId tx)

data WaitResult
  = TxInvalid {transaction :: CardanoTx, reason :: Text}
  | SnapshotConfirmed {transactions :: [Value], snapshotNumber :: Scientific}

data Registry tx = Registry
  { confirmedTxs :: TVar IO (Map.Map (TxId tx) Event)
  , submissionQ :: TBQueue IO CardanoTx
  , latestSnapshot :: TVar IO Scientific
  }

newRegistry ::
  [CardanoTx] ->
  IO (Registry CardanoTx)
newRegistry txs = do
  confirmedTxs <- newTVarIO mempty
  submissionQ <- newTBQueueIO (fromIntegral $ length txs)
  atomically $ mapM_ (writeTBQueue submissionQ) txs
  latestSnapshot <- newTVarIO 0
  pure $ Registry{confirmedTxs, submissionQ, latestSnapshot}

submitTxs ::
  HydraClient ->
  Registry CardanoTx ->
  IO ()
submitTxs client registry@Registry{confirmedTxs, submissionQ} = do
  txToSubmit <- atomically $ tryReadTBQueue submissionQ
  case txToSubmit of
    Just tx -> do
      newTx confirmedTxs client tx
      atomically $ do
        event <- Map.lookup (txId tx) <$> readTVar confirmedTxs
        check (isJust $ confirmedAt =<< event)
      submitTxs client registry
    Nothing -> pure ()

waitForAllConfirmations ::
  HydraClient ->
  Registry CardanoTx ->
  Set (TxId CardanoTx) ->
  IO ()
waitForAllConfirmations n1 Registry{confirmedTxs, submissionQ, latestSnapshot} allIds = do
  go allIds
 where
  go remainingIds
    | Set.null remainingIds = do
      putStrLn "All transactions confirmed. Sweet!"
    | otherwise = do
      waitForSnapshotConfirmation >>= \case
        TxInvalid{transaction} -> do
          putTextLn $ "TxInvalid: " <> show (txId transaction) <> ", resubmitting"
          atomically $ writeTBQueue submissionQ transaction
          go remainingIds
        SnapshotConfirmed{transactions, snapshotNumber} -> do
          -- TODO(SN): use a tracer for this
          atomically $ modifyTVar' latestSnapshot (max snapshotNumber)
          confirmedIds <- mapM (confirmTx confirmedTxs) transactions
          putTextLn $ "Snapshot confirmed: " <> show snapshotNumber
          putTextLn $ "Transaction(s) confirmed: " <> fmtIds confirmedIds
          go $ remainingIds \\ Set.fromList confirmedIds

  waitForSnapshotConfirmation = waitMatch 20 n1 $ \v ->
    maybeTxInvalid v <|> maybeSnapshotConfirmed v

  fmtIds =
    toText . intercalate "" . fmap (("\n    - " <>) . show)

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

analyze :: (TxId CardanoTx, Event) -> Maybe (UTCTime, NominalDiffTime)
analyze = \case
  (_, Event{submittedAt, confirmedAt = Just conf}) -> Just (submittedAt, conf `diffUTCTime` submittedAt)
  _ -> Nothing

writeResultsCsv :: FilePath -> [(UTCTime, NominalDiffTime)] -> IO ()
writeResultsCsv fp res = do
  putStrLn $ "Writing results to: " <> fp
  writeFileLBS fp $ headers <> "\n" <> foldMap toCsv res
 where
  headers = "txId,confirmationTime"

  toCsv (a, b) = show a <> "," <> encode b <> "\n"
