module Main where

import "hydra-prelude" Hydra.Prelude

import Options (Command (..), DepositOptions (..), RecoverOptions (..), parseHydraCommand)
import "aeson" Data.Aeson (eitherDecodeFileStrict)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-cardano-api" Hydra.Cardano.Api (LedgerEra, PParams, TxIx (..), UTxO, textEnvelopeToJSON, toShelleyNetwork, pattern TxIn)
import "hydra-tx" Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import "hydra-tx" Hydra.Tx.Deposit (depositTx, observeDepositTxOut)
import "hydra-tx" Hydra.Tx.Recover (recoverTx)

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
