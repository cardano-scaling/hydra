{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Aeson
import qualified Data.Map as Map
import Data.Time
import Hydra.Ledger as Ledger
import Hydra.Ledger.Cardano as Ledger
import Hydra.Prelude

data ConfTx = ConfTx {ts :: UTCTime, txs :: TxId CardanoTx}
  deriving (Show, Generic, ToJSON, FromJSON)

data Dataset = Dataset
  { initialUtxo :: Utxo CardanoTx
  , transactionsSequence :: [CardanoTx]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | This "script" produces a JSON file mapping size of UTXO set over time.
--
-- It takes exactly 2 arguments:
--  * A JSON file listing each transaction's confirmation time (see @README.md@ for details
--    on how to produce this file),
--  * A JSON file containing the list of 'Dataset' used to run the benchmark (obviously, should
--    be the one that lead to production of the above confirmation time).
--
-- The output file contains a list of pairs of the form @(timestamp,integer)@ denoting the UTXO
-- set size after each transaction is confirmed.
--
-- This is a Haskell script and not a bash script because we need to apply each transaction in
-- order to compute the UTXO set's size as this is not output in the logs. We could add this
-- information to the 'SnapshotConfirmed' event for example, but this seems contrived.
main :: IO ()
main = do
  [confirmedTxsJsonFile, dataset] <- getArgs
  -- assume it's sorted
  Just confs <- decodeFileStrict confirmedTxsJsonFile :: IO (Maybe [ConfTx])
  Just js <- decodeFileStrict dataset :: IO (Maybe [Dataset])
  let txById = Map.fromList $ map (\tx -> (txId tx, tx)) & mconcat (transactionsSequence <$> js)
      confirmedTxs = catMaybes $ map (`Map.lookup` txById) (txs <$> confs)
      app (u, sz) tx = applyTransactions cardanoLedger u [tx] & either (error . show) (\u' -> (u', Ledger.size u' : sz))
      utxo = mconcat (initialUtxo <$> js)
      szs = foldl' app (utxo, [Ledger.size utxo]) confirmedTxs
      utxoSizeByTxId = zip (txId <$> confirmedTxs) $ reverse $ snd szs
  encodeFile "utxo-size.json" utxoSizeByTxId
