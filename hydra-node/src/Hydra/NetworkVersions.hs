{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.NetworkVersions where

import Hydra.Prelude hiding (encodeUtf8)

import Control.Lens ((^@..))
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (members, _Object)
import Data.FileEmbed (embedFile)
import Data.List qualified as List
import Data.Text (pack, splitOn, toLower, unpack)
import Data.Text.Encoding (encodeUtf8)
import Hydra.Cardano.Api (TxId, deserialiseFromRawBytesHex)

networkVersionsFile :: ByteString
networkVersionsFile = $(embedFile "./../networks.json")

parseNetworkTxIds :: String -> Either String [TxId]
parseNetworkTxIds networkString = do
  let networkTxt = toLower $ pack networkString
  let info = networkVersionsFile ^@.. members . _Object
  case find (\(n, _) -> Key.toText n == networkTxt) info of
    Nothing -> Left $ "Unknown network:" <> unpack networkTxt
    Just (_, t) -> getLastTxId t
 where
  getLastTxId t = do
    lastTxIds <-
      case List.last $ KeyMap.elems t of
        String s -> Right s
        _ -> Left "Failed to find the last tx-id string in networks.json"
    mapM parseToTxId (splitOn "," lastTxIds)

  parseToTxId textTxId = do
    case deserialiseFromRawBytesHex $ encodeUtf8 textTxId of
      Left _ -> Left $ "Failed to parse string to TxId: " <> unpack textTxId
      Right txid -> Right txid
