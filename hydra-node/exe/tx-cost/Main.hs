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
import qualified Hydra.Contract.MockHead as MockHead
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
  putTextLn "# UTXO\t% max Mem\t% max CPU"
  forM_ [1 .. 100] $ \numElems -> do
    utxo <- generate (foldMap simplifyUtxo <$> vectorOf numElems genSomeUtxo)
    let (tx, lookupUtxo) = mkFanoutTx utxo
    case evaluateTx tx lookupUtxo of
      (Right (toList -> [Right (Ledger.ExUnits mem cpu)])) -> do
        putTextLn $
          show numElems
            <> "\t"
            <> show (100 * fromIntegral mem / maxMem)
            <> "\t"
            <> show (100 * fromIntegral cpu / maxCpu)
      _ ->
        fail $ "Failed to evaluate transaction with " <> show numElems <> " elements."
 where
  genSomeUtxo = genKeyPair >>= fmap (fmap adaOnly) . genOneUtxoFor . fst
  Ledger.ExUnits (fromIntegral @_ @Double -> maxMem) (fromIntegral @_ @Double -> maxCpu) =
    Ledger._maxTxExUnits pparams

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
        MockHead.Closed 1 (toBuiltin $ hashTxOuts $ toList utxo)
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

mkHeadOutput :: StrictMaybe (Ledger.Data LedgerEra) -> Ledger.Alonzo.TxOut LedgerEra
mkHeadOutput headDatum =
  Ledger.Alonzo.TxOut headAddress headValue headDatumHash
 where
  headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
  headValue = Ledger.inject (Ledger.Coin 2_000_000)
  headDatumHash = Ledger.hashData @LedgerEra <$> headDatum
