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
import Control.Lens (to, (^?))
import Control.Monad.Class.MonadSTM (
  MonadSTM (readTVarIO),
  isEmptyTBQueue,
  modifyTVar,
  modifyTVar',
  newTBQueueIO,
  newTVarIO,
  readTBQueue,
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
  , submittedAtSnapshot :: Scientific
  , confirmedAt :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON)

bench :: FilePath -> Utxo CardanoTx -> [CardanoTx] -> Spec
bench workDir initialUtxo txs =
  specify ("Load test on three local nodes (" <> workDir <> ")") $ do
    registry <- newRegistry txs
    showLogsOnFailure $ \tracer ->
      failAfter 300 $ do
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
  TVar IO (Map.Map Scientific Int) ->
  TVar IO (Map.Map (TxId tx) Event) ->
  HydraClient ->
  Scientific ->
  tx ->
  IO ()
newTx inFlights registry client sn tx = do
  now <- getCurrentTime
  atomically $ do
    modifyTVar inFlights $
      flip Map.alter sn $
        \case
          Nothing -> Just 1
          Just n -> Just (succ n)
    modifyTVar registry $
      Map.insert (txId tx) $
        Event
          { submittedAt = now
          , submittedAtSnapshot = sn
          , confirmedAt = Nothing
          }
  send client $ input "NewTx" ["transaction" .= tx]

data WaitResult
  = TxInvalid {transaction :: CardanoTx, reason :: Text}
  | TxValid {transaction :: CardanoTx}
  | SnapshotConfirmed {transactions :: [Value], snapshotNumber :: Scientific}

data Registry tx = Registry
  { confirmedTxs :: TVar IO (Map.Map (TxId tx) Event)
  , submissionQ :: TBQueue IO CardanoTx
  , latestSnapshot :: TVar IO Scientific
  , inFlight :: TVar IO (Map Scientific Int)
  }

newRegistry ::
  [CardanoTx] ->
  IO (Registry CardanoTx)
newRegistry txs = do
  confirmedTxs <- newTVarIO mempty
  submissionQ <- newTBQueueIO (fromIntegral $ length txs)
  atomically $ mapM_ (writeTBQueue submissionQ) txs
  latestSnapshot <- newTVarIO 0
  inFlight <- newTVarIO mempty
  pure $ Registry{confirmedTxs, submissionQ, latestSnapshot, inFlight}

submitTxs ::
  HydraClient ->
  Registry CardanoTx ->
  IO ()
submitTxs client registry@Registry{confirmedTxs, submissionQ, latestSnapshot, inFlight} = do
  (queueEmpty, unconfirmedIds) <- atomically $ do
    unconfirmedIds <- Map.keys . Map.filter (isNothing . confirmedAt) <$> readTVar confirmedTxs
    queueEmpty <- isEmptyTBQueue submissionQ
    pure (queueEmpty, unconfirmedIds)

  if
      | queueEmpty && null unconfirmedIds -> do
        putStrLn "Done submitting transactions."
      | queueEmpty -> do
        threadDelay 0.1 >> submitTxs client registry
      | otherwise -> do
        -- NOTE: We control whether the server is 'overloaded' / flooded with
        -- invalid transactions by looking at how many transactions are rejected
        -- in the current snapshot. After a certain threshold T, we consider the
        -- server overloaded for this particular snapshot and we stop submitting
        -- transactions until the next snapshot.
        --
        -- Note that, we only consider the server overloaded if _at least one_
        -- transaction was marked as valid. This ensure that we don't end up in
        -- a situation where we dead-lock ourselves because the only valid
        -- transaction was a bit far in the submission queue. So there are cases
        -- where we may submit way more than T invalid transactions. Beside, since
        -- the transaction submissions and confirmations are asynchronous, we
        -- will typically send more than T transaction before observing T failures.
        let maxInFlight = 100
        (isOverloaded, sn) <- atomically $ do
          sn <- readTVar latestSnapshot
          n <- fromMaybe 0 . Map.lookup sn <$> readTVar inFlight
          pure (n > maxInFlight, sn)

        if isOverloaded
          then threadDelay 1
          else do
            (tx, txs) <-
              atomically $
                (,)
                  <$> readTBQueue submissionQ
                  <*> readTVar confirmedTxs

            case Map.lookup (txId tx) txs of
              Nothing -> do
                newTx inFlight confirmedTxs client sn tx
              Just e | submittedAtSnapshot e < sn -> do
                newTx inFlight confirmedTxs client sn tx
              _ -> do
                atomically $ writeTBQueue submissionQ tx

        submitTxs client registry

waitForAllConfirmations ::
  HydraClient ->
  Registry CardanoTx ->
  Set (TxId CardanoTx) ->
  IO ()
waitForAllConfirmations n1 Registry{confirmedTxs, submissionQ, latestSnapshot, inFlight} allIds = do
  go allIds
 where
  go remainingIds
    | Set.null remainingIds = do
      putStrLn "All transactions confirmed. Sweet!"
    | otherwise = do
      waitForSnapshotConfirmation >>= \case
        TxValid{} -> do
          atomically $ do
            sn <- readTVar latestSnapshot
            modifyTVar' inFlight (Map.adjust pred sn)
          go remainingIds
        TxInvalid{transaction} -> do
          putTextLn $ "TxInvalid: " <> show (txId transaction) <> ", resubmitting"
          atomically $ do
            sn <- readTVar latestSnapshot
            modifyTVar' inFlight (Map.adjust pred sn)
            writeTBQueue submissionQ transaction
          go remainingIds
        SnapshotConfirmed{transactions, snapshotNumber} -> do
          putStrLn "SnapshotCONFIRMED"
          -- TODO(SN): use a tracer for this
          atomically $ modifyTVar' latestSnapshot (max snapshotNumber)
          confirmedIds <- mapM (confirmTx confirmedTxs) transactions
          putTextLn $ "Snapshot confirmed: " <> show snapshotNumber
          putTextLn $ "Transaction(s) confirmed: " <> fmtIds confirmedIds
          go $ remainingIds \\ Set.fromList confirmedIds

  waitForSnapshotConfirmation = waitMatch 20 n1 $ \v ->
    maybeTxInvalid v <|> maybeTxValid v <|> maybeSnapshotConfirmed v

  fmtIds =
    toText . intercalate "" . fmap (("\n    - " <>) . show)

  maybeTxInvalid v = do
    guard (v ^? key "tag" == Just "TxInvalid")
    v ^? key "transaction" . to fromJSON >>= \case
      Error _ -> Nothing
      Success tx ->
        TxInvalid tx <$> v ^? key "validationError" . key "reason" . _String

  maybeTxValid v = do
    guard (v ^? key "tag" == Just "TxValid")
    v ^? key "transaction" . to fromJSON >>= \case
      Error _ -> Nothing
      Success tx -> Just (TxValid tx)

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
