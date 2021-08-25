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
  modifyTVar,
  newTVarIO,
 )
import Data.Aeson (Result (Error, Success), Value, encode, fromJSON, (.=))
import Data.Aeson.Lens (key, _Array, _Number, _String)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.Set ((\\))
import qualified Data.Set as Set
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
  it ("Benchmarks three local nodes in " <> workDir) $ do
    registry <- newTVarIO mempty :: IO (TVar IO (Map.Map (TxId CardanoTx) Event))
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

                for_ txs (\tx -> newTx registry n1 tx >> threadDelay 0.001)
                  `concurrently_` waitForAllConfirmations n1 registry txs

                send n1 $ input "Close" []
                waitMatch (contestationPeriod + 3) n1 $ \v ->
                  guard (v ^? key "tag" == Just "HeadIsFinalized")

    res <- mapMaybe analyze . Map.toList <$> readTVarIO registry
    writeResultsCsv (workDir </> "results.csv") res

--
-- Helpers
--

noUtxos :: Utxo CardanoTx
noUtxos = mempty

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

data WaitResult
  = TxInvalid {transaction :: CardanoTx, reason :: Text}
  | SnapshotConfirmed {transactions :: [Value], snapshotNumber :: Scientific}

waitForAllConfirmations :: HydraClient -> TVar IO (Map.Map (TxId CardanoTx) Event) -> [CardanoTx] -> IO ()
waitForAllConfirmations n1 registry txs =
  go allIds
 where
  allIds = Set.fromList $ map txId txs

  go remainingIds
    | Set.null remainingIds = pure ()
    | otherwise = do
      waitForSnapshotConfirmation >>= \case
        TxInvalid{transaction, reason} -> do
          atomically $ modifyTVar registry $ Map.delete (txId transaction)
          putTextLn $ "TxInvalid: " <> show (txId transaction) <> "\nReason: " <> reason
          go $ Set.delete (txId transaction) remainingIds
        SnapshotConfirmed{transactions, snapshotNumber} -> do
          -- TODO(SN): use a tracer for this
          putTextLn $ "Snapshot confirmed: " <> show snapshotNumber
          confirmedIds <- mapM (confirmTx registry) transactions
          go $ remainingIds \\ Set.fromList confirmedIds

  waitForSnapshotConfirmation = waitMatch 20 n1 $ \v ->
    maybeTxInvalid v <|> maybeSnapshotConfirmed v

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

  toCsv (a, b) = encode a <> "," <> encode b <> "\n"
