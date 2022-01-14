{-# LANGUAGE TypeApplications #-}

import Hydra.Prelude

import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Val as Ledger
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Hydra.Chain.Direct.Tx (fanoutTx, plutusScript, policyId, scriptAddr)
import qualified Hydra.Contract.Head as Head
import Hydra.Ledger.Cardano (
  CardanoTx,
  LedgerEra,
  Utxo,
  adaOnly,
  fromLedgerTx,
  fromLedgerUtxo,
  genKeyPair,
  genOneUtxoFor,
  hashTxOuts,
  simplifyUtxo,
 )
import Hydra.Ledger.Cardano.Evaluate (evaluateTx, pparams)
import Plutus.V1.Ledger.Api (toBuiltin, toData)
import Test.QuickCheck (generate, vectorOf)

main :: IO ()
main = do
  putStrLn "Cost of running the fanout validator"
  putStrLn "# UTXO  % max Mem   % max CPU"
  forM_ [1 .. 100] $ \numElems -> do
    utxo <- generate (foldMap simplifyUtxo <$> vectorOf numElems genSomeUtxo)
    let (tx, lookupUtxo) = mkFanoutTx utxo
    case evaluateTx tx lookupUtxo of
      (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) -> do
        putStrLn $
          showPad 8 numElems
            <> showPad 12 (100 * fromIntegral mem / maxMem)
            <> showPad 12 (100 * fromIntegral cpu / maxCpu)
      _ ->
        fail $ "Failed to evaluate transaction with " <> show numElems <> " elements."
 where
  genSomeUtxo = genKeyPair >>= fmap (fmap adaOnly) . genOneUtxoFor . fst
  Ledger.ExUnits (fromIntegral @_ @Double -> maxMem) (fromIntegral @_ @Double -> maxCpu) =
    Ledger._maxTxExUnits pparams

showPad :: Show a => Int -> a -> String
showPad n x =
  show x <> replicate (n - len) ' '
 where
  len = length $ show @String x

mkFanoutTx :: Utxo -> (CardanoTx, Utxo)
mkFanoutTx utxo =
  ( fromLedgerTx tx
  , fromLedgerUtxo lookupUtxo
  )
 where
  tx = fanoutTx utxo (headInput, headDatum)
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum =
    Ledger.Data $
      toData $
        Head.Closed 1 (toBuiltin $ hashTxOuts $ toList utxo)
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

mkHeadOutput :: StrictMaybe (Ledger.Data LedgerEra) -> Ledger.Alonzo.TxOut LedgerEra
mkHeadOutput headDatum =
  Ledger.Alonzo.TxOut headAddress headValue headDatumHash
 where
  headAddress = scriptAddr $ plutusScript $ Head.validatorScript policyId
  headValue = Ledger.inject (Ledger.Coin 2_000_000)
  headDatumHash = Ledger.hashData @LedgerEra <$> headDatum
