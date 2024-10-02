{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens (asIndex, to, (&), (^.), (^?))
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import Data.String
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (decodeLatin1)
import Shh

getAddress :: IO ByteString
getAddress = do
  exe
    "cardano-cli"
    "address"
    "build"
    "--payment-verification-key-file"
    "devnet/credentials/alice-funds.vk"
    "--testnet-magic"
    "42"
    |> capture

getUTxO :: ByteString -> IO (Either String Value)
getUTxO address = do
  k <-
    exe
      "cardano-cli"
      "query"
      "utxo"
      "--socket-path"
      "devnet/node.socket"
      "--address"
      address
      "--testnet-magic"
      "42"
      "--output-json"
      |> capture
  pure $ eitherDecode k

buildTransaction :: ByteString -> ByteString -> IO (Either String Value)
buildTransaction i o = do
  x <-
    exe
      "cardano-cli"
      "conway"
      "transaction"
      "build-raw"
      "--tx-in"
      i
      "--tx-out"
      (o <> "+5000000")
      "--fee"
      "0"
      "--out-file"
      "/dev/stdout"
      |> capture
  pure $ eitherDecode x

blueprintTx :: Text -> Value -> Text -> Value
blueprintTx address utxo cborHex =
  object
    [ "blueprintTx"
        .= object
          [ "cborHex" .= String cborHex
          , "description" .= ""
          , "type" .= "Tx ConwayEra"
          ]
    , "utxo" .= utxo
    ]

decodeTxFile :: IO (Either String Value)
decodeTxFile = do
  x <- LBS.readFile "tx.json"
  pure $ eitherDecode x

curlCommitRequest :: IO ByteString
curlCommitRequest =
  exe
    "curl"
    "-X"
    "POST"
    "127.0.0.1:4001/commit"
    "--data"
    "@commit-request.json"
    |> capture

signTransaction :: IO ()
signTransaction =
  exe
    "cardano-cli"
    "transaction"
    "sign"
    "--tx-file"
    "commit-tx.json"
    "--signing-key-file"
    "devnet/credentials/alice-funds.sk"
    "--out-file"
    "signed-tx.json"

submitTransaction :: IO ()
submitTransaction =
  exe
    "cardano-cli"
    "transaction"
    "submit"
    "--tx-file"
    "signed-tx.json"
    "--socket-path"
    "devnet/node.socket"
    "--testnet-magic"
    "42"

main = do
  address <- getAddress
  utxo <- either error id <$> getUTxO address
  let utxoKey = utxo ^. members . asIndex
  tx <- either error id <$> buildTransaction (Data.String.fromString . Key.toString $ utxoKey) address
  let cborHex = tx ^? key "cborHex"
  let (String cborHexBS) = fromJust cborHex
  encodeFile "commit-request.json" $
    blueprintTx
      (Text.toStrict $ decodeLatin1 address)
      utxo
      cborHexBS
  commitTx <- curlCommitRequest
  LBS.writeFile "commit-tx.json" commitTx
  signTransaction
  submitTransaction
