-- | Utility functions to filter and simplify raw logs.
module Hydra.LogFilter where

import Control.Lens (Traversal', at, (.~), (?~), (^..), (^?))
import Data.Aeson (Value (Array, Null))
import Data.Aeson.Lens (key, values, _Object)
import Hydra.Prelude

filterLog :: Value -> Maybe Value
filterLog entry = do
  guard (entry ^? key "message" . key "tag" == Just "Node")
  node <- entry ^? key "message" . key "node"
  result <- case node ^? key "tag" of
    Just "BeginEvent" -> do
      case (node ^? networkMessage . key "tag") <|> (node ^? clientEvent . key "tag") of
        Just "ReqTx" -> processReqTx node
        Just "NewTx" -> replaceTransactionWithTxId node clientEvent
        Just "ReqSn" -> replaceTransactionsWithTxIds node networkMessage
        _ -> pure node
    Just "EndEvent" -> do
      case (node ^? networkMessage . key "tag") <|> (node ^? clientEvent . key "tag") of
        Just "ReqTx" -> processReqTx node
        Just "NewTx" -> replaceTransactionWithTxId node clientEvent
        Just "ReqSn" -> replaceTransactionsWithTxIds node networkMessage
        _ -> pure node
    Just "BeginEffect" -> do
      case node ^? clientEffect . key "tag" <|> node ^? networkEffect . key "tag" of
        Just "SnapshotConfirmed" -> processSnapshotConfirmed node
        Just "Committed" -> replaceUtxoWithNull node
        Just "TxSeen" -> replaceTransactionWithTxId node clientEffect
        Just "TxInvalid" -> replaceTransactionWithTxId node clientEffect >>= replaceUtxoWithNull
        Just "TxValid" -> replaceTransactionWithTxId node clientEffect
        Just "ReqTx" -> replaceTransactionWithTxId node networkEffect
        Just "ReqSn" -> replaceTransactionsWithTxIds node networkEffect
        _ -> pure node
    Just "EndEffect" -> do
      case node ^? clientEffect . key "tag" <|> node ^? networkEffect . key "tag" of
        Just "SnapshotConfirmed" -> processSnapshotConfirmed node
        Just "Committed" -> replaceUtxoWithNull node
        Just "TxSeen" -> replaceTransactionWithTxId node clientEffect
        Just "TxInvalid" -> replaceTransactionWithTxId node clientEffect >>= replaceUtxoWithNull
        Just "TxValid" -> replaceTransactionWithTxId node clientEffect
        Just "ReqTx" -> replaceTransactionWithTxId node networkEffect
        Just "ReqSn" -> replaceTransactionsWithTxIds node networkEffect
        _ -> pure node
    _ -> pure node
  pure $ entry & _Object . at "message" ?~ result
 where
  processReqTx node = do
    txid <- node ^? networkMessage . key "transaction" . key "id"
    pure $ node & networkMessage . key "transaction" .~ txid

  replaceTransactionsWithTxIds :: Value -> Traversal' Value Value -> Maybe Value
  replaceTransactionsWithTxIds node path = do
    let txids = node ^.. path . key "transactions" . values . key "id"
    pure $ node & path . key "transactions" .~ Array (fromList txids)

  processSnapshotConfirmed node = do
    let txids = node ^.. clientEffect . key "snapshot" . key "confirmedTransactions" . values . key "id"
    pure $
      node
        & clientEffect . key "snapshot" . key "confirmedTransactions" .~ Array (fromList txids)
        & clientEffect . key "snapshot" . key "utxo" .~ Null

  replaceUtxoWithNull node = pure $ node & clientEffect . key "utxo" .~ Null

  replaceTransactionWithTxId :: Value -> Traversal' Value Value -> Maybe Value
  replaceTransactionWithTxId node path = do
    txid <- node ^? path . key "transaction" . key "id"
    pure $ node & path . key "transaction" .~ txid

  networkMessage :: Traversal' Value Value
  networkMessage = key "event" . key "message"

  networkEffect :: Traversal' Value Value
  networkEffect = key "effect" . key "message"

  clientEvent :: Traversal' Value Value
  clientEvent = key "event" . key "clientInput"

  clientEffect :: Traversal' Value Value
  clientEffect = key "effect" . key "serverOutput"
