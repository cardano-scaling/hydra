{-# LANGUAGE TypeApplications #-}

module Hydra.Painter where

import Control.Exception (IOException)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Hydra.Cardano.Api
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.ClientInput (ClientInput (GetUTxO, NewTx))
import Hydra.Ledger.Cardano (emptyTxBody)
import Hydra.Network (Host (..))
import Hydra.Prelude
import Hydra.ServerOutput (ServerOutput (GetUTxOResponse))
import Network.WebSockets (
  Connection,
  runClient,
  sendTextData,
 )
import Network.WebSockets.Connection (receive, receiveData)

data Pixel = Pixel
  { x, y, red, green, blue :: Word8
  }

paintPixel :: FilePath -> Connection -> Pixel -> IO ()
paintPixel signingKeyPath cnx pixel = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) signingKeyPath
  let vk = getVerificationKey sk
  flushQueue cnx
  sendTextData @Text cnx $ decodeUtf8 $ Aeson.encode (GetUTxO @Tx)
  msg <- receiveData cnx
  putStrLn $ "Received from Hydra-node: " <> show msg
  case Aeson.eitherDecode @(ServerOutput Tx) msg of
    Left e -> error $ "Failed to decode server answer:  " <> show e
    Right (GetUTxOResponse (UTxO utxo)) -> do
      let myAddress = mkVkAddress networkId vk
          (txIn, txOut) = Map.findMin $ Map.filter (\TxOut{txOutAddress = addr} -> addr == myAddress) utxo
      case mkPaintTx (txIn, txOut) (myAddress, txOutValue txOut) sk pixel of
        Left err -> error $ "failed to build pixel transaction " <> show err
        Right tx -> sendTextData cnx $ Aeson.encode $ NewTx tx
    Right other -> error $ "Unexpected server answer:  " <> decodeUtf8 msg
 where
  networkId = Testnet unusedNetworkMagic
  unusedNetworkMagic = NetworkMagic 42

  flushQueue cnx =
    race_ (threadDelay 0.25) (void (receive cnx) >> flushQueue cnx)

withClient :: Host -> (Connection -> IO ()) -> IO ()
withClient Host{hostname, port} action = retry
 where
  retry = runClient (toString hostname) (fromIntegral port) "/" action `catch` \(_ :: IOException) -> retry

-- | Create a zero-fee, payment cardano transaction.
mkPaintTx ::
  (TxIn, TxOut CtxUTxO) ->
  -- | Recipient address and amount.
  (AddressInEra, Value) ->
  -- | Sender's signing key.
  SigningKey PaymentKey ->
  Pixel ->
  Either TxBodyError Tx
mkPaintTx (txin, txOutBefore) (recipient, valueOut) sk Pixel{x, y, red, green, blue} = do
  body <- makeTransactionBody bodyContent
  let witnesses = [makeShelleyKeyWitness body (WitnessPaymentKey sk)]
  pure $ makeSignedTransaction witnesses body
 where
  metadata = TxMetadataInEra $ TxMetadata $ Map.fromList [(14, listOfInts)]

  listOfInts = TxMetaList $ TxMetaNumber . fromIntegral <$> [x, y, red, green, blue]

  TxOut{txOutValue = valueIn} = txOutBefore

  bodyContent =
    emptyTxBody
      { txIns = map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin]
      , txOuts = outs
      , txFee = TxFeeExplicit fee
      , txMetadata = metadata
      }

  outs =
    TxOut @CtxTx recipient valueOut TxOutDatumNone ReferenceScriptNone :
      [ toTxContext $ txOutBefore{txOutValue = valueIn <> negateValue valueOut}
      | valueOut /= valueIn
      ]

  fee = Lovelace 0
