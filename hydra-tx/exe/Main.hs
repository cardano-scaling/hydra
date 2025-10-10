module Main where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson (eitherDecodeFileStrict)
import Hydra.Cardano.Api (LedgerEra, PParams, TxIx (..), textEnvelopeToJSON, toShelleyNetwork, pattern TxIn)
import Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import Hydra.Tx.Deposit (depositTx, observeDepositTxOut)
import Hydra.Tx.Recover (recoverTx)
import Options (Command (..), DepositOptions (..), RecoverOptions (..), parseHydraCommand)

main :: IO ()
main =
  parseHydraCommand >>= \case
    Deposit DepositOptions{networkId, pparamsFilePath, headId, outFile, utxoFilePath, depositSlotNo, depositDeadline} ->
      eitherDecodeFileStrict pparamsFilePath >>= \case
        Left err -> die $ "failed to parse protocol parameters! " <> err
        Right (pparams :: PParams LedgerEra) ->
          eitherDecodeFileStrict utxoFilePath >>= \case
            Left err -> die $ "failed to parse provided UTXO file! " <> err
            Right (utxo :: UTxO) -> do
              writeFileLBS outFile
                . textEnvelopeToJSON Nothing
                $ depositTx networkId pparams headId (mkSimpleBlueprintTx utxo) depositSlotNo depositDeadline Nothing
              putStrLn $ "Wrote deposit transaction to " <> outFile
    Recover RecoverOptions{networkId, outFile, recoverTxId, utxoFilePath, recoverSlotNo} -> do
      -- XXX: Only requires network discriminator / not networkId
      let network = toShelleyNetwork networkId
      eitherDecodeFileStrict utxoFilePath >>= \case
        Left err -> die $ "failed to parse provided UTXO file! " <> err
        Right (utxo :: UTxO) -> do
          case UTxO.resolveTxIn (TxIn recoverTxId (TxIx 0)) utxo of
            Nothing -> die "failed to resolve deposited UTxO with provided TxIn"
            Just depositedTxOut -> do
              case observeDepositTxOut network depositedTxOut of
                Nothing -> die "Failed to observe deposit UTxO"
                Just (_, deposited, _) -> do
                  let recoverTransaction = recoverTx recoverTxId deposited recoverSlotNo
                  writeFileLBS outFile $ textEnvelopeToJSON Nothing recoverTransaction
                  putStrLn $ "Wrote deposit transaction to " <> outFile
