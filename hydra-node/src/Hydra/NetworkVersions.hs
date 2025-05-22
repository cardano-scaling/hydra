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
import Data.Text (splitOn)
import Data.Text.Encoding (encodeUtf8)
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), TxId, deserialiseFromRawBytesHex)

networkVersionsFile :: ByteString
networkVersionsFile = $(embedFile "./../networks.json")

data ParseError
  = ExpectedStringForTxId
  | FailedToParseTextToTxId Text
  | UnknownNetwork Text
  deriving stock (Eq, Show)

instance Exception ParseError

parseNetworkVersions :: ByteString -> IO [(NetworkId, [TxId])]
parseNetworkVersions bs = do
  let info = bs ^@.. members . _Object
  fmap catMaybes <$> forM info $ \(n, t) -> do
    let textKey = Key.toText n
    case textKey of
      "mainnet" -> do
        txids <- getLastTxId t
        pure $ Just (Mainnet, txids)
      "preview" -> do
        txids <- getLastTxId t
        pure $ Just (Testnet $ NetworkMagic 2, txids)
      "preprod" -> do
        txids <- getLastTxId t
        pure $ Just (Testnet $ NetworkMagic 1, txids)
      _ -> pure Nothing
 where
  getLastTxId t = do
    lastTxIds <-
      case List.last $ KeyMap.elems t of
        String s -> pure s
        _ -> throwIO ExpectedStringForTxId
    mapM parseToTxId (splitOn "," lastTxIds)

  parseToTxId textTxId = do
    case deserialiseFromRawBytesHex $ encodeUtf8 textTxId of
      Left _ -> throwIO $ FailedToParseTextToTxId textTxId
      Right txid -> pure txid
