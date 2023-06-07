-- | Utility functions to filter and simplify raw logs.
module Hydra.LogFilter where

import Hydra.Prelude

import Control.Lens (Traversal', at, (.~), (?~), (^..), (^?))
import Data.Aeson (Value (Array, Null), decode, object, (.=))
import Data.Aeson.Lens (key, values, _Object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.Cardano.Api (Tx)
import Hydra.HeadLogic (Event (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Network.Message (Message)
import Hydra.Node (HydraNodeLog (BeginEvent, EndEvent))

tracePerformance :: LBS.ByteString -> [Value]
tracePerformance =
  computeDuration . mapMaybe decode . LBS.splitWith ((== '\n') . toEnum . fromIntegral)

computeDuration :: [Envelope (HydraLog Tx (Message Tx))] -> [Value]
computeDuration = snd . foldl' analyse (mempty, mempty)
 where
  analyse :: (Map Word64 (TxIdType Tx, UTCTime), [Value]) -> Envelope (HydraLog Tx (Message Tx)) -> (Map Word64 (TxIdType Tx, UTCTime), [Value])
  analyse (pending, output) = \case
    (Envelope ut n txt (Node (BeginEvent pa eventID (ClientEvent (NewTx tx))))) -> (Map.insert eventID (txId tx, ut) pending, output)
    (Envelope ut n txt (Node (EndEvent pa eventID))) ->
      let (txid, begin) = fromJust $ Map.lookup eventID pending
       in ( Map.delete eventID pending
          , object
              [ "timestamp" .= begin
              , "id" .= txid
              , "event" .= ("NewTx" :: Text)
              , "us" .= (1_000_000 * diffUTCTime ut begin)
              ] :
            output
          )
    _ -> (pending, output)

-- | Generate "traces" for various events
--
-- @@
--  {
--    "timestamp": "....",
--    "event": "NewTx",
--    "id": "123123",
--    "duration": "42"
--  }
--  {
--    "timestamp": "....",
--    "event": "ReqTx",
--    "id": "123123",
--    "duration": "12"
--  }
-- @@
filterEntry :: Envelope (HydraLog Tx (Message Tx)) -> Maybe Value
filterEntry (Envelope ut n txt (Node (BeginEvent pa wo (ClientEvent (NewTx tx))))) = Nothing
filterEntry (Envelope ut n txt (Node (BeginEvent pa wo (NetworkEvent nat mes)))) = Nothing
filterEntry (Envelope ut n txt (Node (EndEvent pa wo))) = Nothing
filterEntry _ = Nothing

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
        Just "TxInvalid" -> replaceTransactionWithTxId node clientEffect >>= replaceUtxoWithNull
        Just "TxValid" -> replaceTransactionWithTxId node clientEffect
        Just "ReqTx" -> replaceTransactionWithTxId node networkEffect
        Just "ReqSn" -> replaceTransactionsWithTxIds node networkEffect
        _ -> pure node
    Just "EndEffect" -> do
      case node ^? clientEffect . key "tag" <|> node ^? networkEffect . key "tag" of
        Just "SnapshotConfirmed" -> processSnapshotConfirmed node
        Just "Committed" -> replaceUtxoWithNull node
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
