module Main where

import Hydra.Cardano.Api (networkIdToNetwork, textEnvelopeToJSON)
import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson (eitherDecodeFileStrict)
import Hydra.Tx.Deposit (depositTx, observeDepositTxOut)
import Hydra.Tx.Recover (recoverTx)
import Options

main :: IO ()
main = do
  cmd <- parseHydraCommand
  case cmd of
    Deposit DepositOptions{networkId, headId, outFile, utxoFilePath, depositDeadline} ->
      eitherDecodeFileStrict utxoFilePath >>= \case
        Left err -> die $ "failed to parse provided UTXO file! " <> err
        Right (utxo :: UTxO) -> do
          let depositTransaction = depositTx networkId headId utxo depositDeadline
          writeFileLBS outFile $ textEnvelopeToJSON Nothing depositTransaction
          putStrLn $ "Wrote deposit transaction to " <> outFile
    Recover RecoverOptions{networkId, outFile, recoverTxIn, utxoFilePath, recoverSlotNo} -> do
      -- XXX: Only requires network discriminator / not networkId
      let network = networkIdToNetwork networkId
      eitherDecodeFileStrict utxoFilePath >>= \case
        Left err -> die $ "failed to parse provided UTXO file! " <> err
        Right (utxo :: UTxO) -> do
          case UTxO.resolve recoverTxIn utxo of
            Nothing -> die "failed to resolve deposited UTxO with provided TxIn"
            Just depositedTxOut -> do
              case observeDepositTxOut network depositedTxOut of
                Nothing -> die "Failed to observe deposit UTxO"
                Just (_, deposited, _) -> do
                  let recoverTransaction = recoverTx recoverTxIn deposited recoverSlotNo
                  writeFileLBS outFile $ textEnvelopeToJSON Nothing recoverTransaction
                  putStrLn $ "Wrote deposit transaction to " <> outFile
