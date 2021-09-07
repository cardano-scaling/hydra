{-# LANGUAGE TypeApplications #-}

import Control.Lens (Traversal', (.~), (^?))
import Data.Aeson (Value (Null), decode, encode)
import Data.Aeson.Lens (key, _Array)
import qualified Data.ByteString.Char8 as LBS
import qualified Data.ByteString.Lazy as LBS
import Hydra.Prelude
import System.IO.Error (isEOFError)

main :: IO ()
main = do
  [logFile] <- getArgs
  withFile logFile ReadMode $ \hdl -> go hdl
 where
  go hdl =
    try (LBS.hGetLine hdl) >>= \case
      Left err | isEOFError err -> pure ()
      Left err -> throwIO err
      Right line -> do
        case filterLog =<< decode @Value (LBS.fromStrict line) of
          Nothing -> pure ()
          Just v -> LBS.hPutStrLn stdout (LBS.toStrict $ encode v)
        go hdl

filterLog :: Value -> Maybe Value
filterLog entry = do
  guard (entry ^? key "message" . key "tag" == Just "Node")
  node <- entry ^? key "message" . key "node"
  tag <- node ^? key "tag"
  case tag of
    "ProcessingEvent" -> do
      case node ^? networkMessage . key "tag" of
        Just "ReqTx" -> processReqTx node
        Just "ReqSn" -> processReqSn node
        _ -> pure node
    "ProcessedEvent" -> do
      case node ^? networkMessage . key "tag" of
        Just "ReqTx" -> processReqTx node
        Just "ReqSn" -> processReqSn node
        _ -> pure node
    "ProcessingEffect" -> do
      case node ^? clientEffect . key "tag" of
        Just "SnapshotConfirmed" -> processSnapshotConfirmed node
        Just "Committed" -> replaceUtxoWithNull node
        Just "TxSeen" -> replaceTransactionWithTxId node
        Just "TxInvalid" -> replaceTransactionWithTxId node >>= replaceUtxoWithNull
        _ -> pure node
    "ProcessedEffect" -> do
      case node ^? clientEffect . key "tag" of
        Just "SnapshotConfirmed" -> processSnapshotConfirmed node
        Just "Committed" -> replaceUtxoWithNull node
        Just "TxSeen" -> replaceTransactionWithTxId node
        Just "TxInvalid" -> replaceTransactionWithTxId node >>= replaceUtxoWithNull
        _ -> pure node
    _ -> pure node
 where
  processReqTx node = do
    txid <- node ^? networkMessage . key "transaction" . key "id"
    pure $ node & networkMessage . key "transaction" .~ txid

  processReqSn node = do
    txids <- node ^? networkMessage . key "transactions" . _Array . traverse . key "id"
    pure $ node & networkMessage . key "transactions" .~ txids

  processSnapshotConfirmed node = do
    txids <- node ^? clientEffect . key "snapshot" . key "confirmedTransactions" . _Array . traverse . key "id"
    pure $
      node
        & clientEffect . key "snapshot" . key "confirmedTransactions" .~ txids
        & clientEffect . key "snapshot" . key "utxo" .~ Null

  replaceUtxoWithNull node = pure $ node & clientEffect . key "utxo" .~ Null

  replaceTransactionWithTxId node = do
    txid <- node ^? clientEffect . key "transaction" . key "id"
    pure $ node & clientEffect . key "transaction" .~ txid

  networkMessage :: Traversal' Value Value
  networkMessage = key "event" . key "message"

  clientEffect :: Traversal' Value Value
  clientEffect = key "effect" . key "serverOutput"
