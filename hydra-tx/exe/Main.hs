module Main where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson (eitherDecodeFileStrict)
import Hydra.Cardano.Api (TxIx (..), networkIdToNetwork, textEnvelopeToJSON, txSpendingUTxO, pattern TxIn)
import Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import Hydra.Tx.Deposit (depositTx, observeDepositTxOut)
import Hydra.Tx.Recover (recoverTx)
import Options (Command (..), DepositOptions (..), RecoverOptions (..), parseHydraCommand)

main :: IO ()
main =
  parseHydraCommand >>= \case
    Deposit DepositOptions{networkId, headId, outFile, utxoFilePath, depositDeadline} ->
      eitherDecodeFileStrict utxoFilePath >>= \case
        Left err -> die $ "failed to parse provided UTXO file! " <> err
        Right (utxo :: UTxO) -> do
          let blueprintTx = txSpendingUTxO utxo
          let commitBlueprint = CommitBlueprintTx{lookupUTxO = utxo, blueprintTx}
          let depositTransaction = depositTx networkId headId commitBlueprint depositDeadline
          writeFileLBS outFile $ textEnvelopeToJSON Nothing depositTransaction
          putStrLn $ "Wrote deposit transaction to " <> outFile
    Recover RecoverOptions{networkId, outFile, recoverTxId, utxoFilePath, recoverSlotNo} -> do
      -- XXX: Only requires network discriminator / not networkId
      let network = networkIdToNetwork networkId
      eitherDecodeFileStrict utxoFilePath >>= \case
        Left err -> die $ "failed to parse provided UTXO file! " <> err
        Right (utxo :: UTxO) -> do
          case UTxO.resolve (TxIn recoverTxId (TxIx 0)) utxo of
            Nothing -> die "failed to resolve deposited UTxO with provided TxIn"
            Just depositedTxOut -> do
              case observeDepositTxOut network depositedTxOut of
                Nothing -> die "Failed to observe deposit UTxO"
                Just (_, deposited, _) -> do
                  let recoverTransaction = recoverTx recoverTxId deposited recoverSlotNo
                  writeFileLBS outFile $ textEnvelopeToJSON Nothing recoverTransaction
                  putStrLn $ "Wrote deposit transaction to " <> outFile
