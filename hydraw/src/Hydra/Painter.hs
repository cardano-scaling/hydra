module Hydra.Painter where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Control.Exception (IOException)
import Data.Aeson qualified as Aeson
import Hydra.API.ClientInput (ClientInput (GetUTxO, NewTx))
import Hydra.Chain.Direct.State ()
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.HeadLogic.Outcome (StateChanged (GetUTxOResponse))
import Hydra.Network (Host (..))
import Network.WebSockets (Connection, runClient, sendTextData)
import Network.WebSockets.Connection (receive, receiveData)

data Pixel = Pixel
  { x, y, red, green, blue :: Word8
  }

paintPixel :: NetworkId -> FilePath -> Connection -> Pixel -> IO ()
paintPixel networkId signingKeyPath cnx pixel = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) signingKeyPath
  let myAddress = mkVkAddress networkId $ getVerificationKey sk
  flushQueue
  sendTextData @Text cnx $ decodeUtf8 $ Aeson.encode (GetUTxO @Tx)
  msg <- receiveData cnx
  putStrLn $ "Received from hydra-node: " <> show msg
  case Aeson.eitherDecode @(StateChanged Tx) msg of
    Right (GetUTxOResponse _ utxo) ->
      case UTxO.find (\TxOut{txOutAddress} -> txOutAddress == myAddress) utxo of
        Nothing -> fail $ "No UTxO owned by " <> show myAddress
        Just (txIn, txOut) ->
          case mkPaintTx (txIn, txOut) sk pixel of
            Right tx -> sendTextData cnx $ Aeson.encode $ NewTx tx
            Left err -> fail $ "Failed to build pixel transaction " <> show err
    Right _ -> fail $ "Unexpected server answer:  " <> decodeUtf8 msg
    Left e -> fail $ "Failed to decode server answer:  " <> show e
 where
  flushQueue =
    race_ (threadDelay 0.25) (void (receive cnx) >> flushQueue)

-- | Same as 'withClient' except we don't retry if connection fails.
withClientNoRetry :: Host -> (Connection -> IO ()) -> IO ()
withClientNoRetry Host{hostname, port} action =
  runClient (toString hostname) (fromIntegral port) "/" action
    `catch` \(e :: IOException) -> print e >> threadDelay 1

withClient :: Host -> (Connection -> IO ()) -> IO ()
withClient Host{hostname, port} action =
  retry
 where
  retry = do
    putTextLn $ "Connecting to Hydra API on " <> hostname <> ":" <> show port <> ".."
    runClient (toString hostname) (fromIntegral port) "/" action
      `catch` \(e :: IOException) -> print e >> threadDelay 1 >> retry

-- | Create a zero-fee, payment cardano transaction with pixel metadata, which
-- just re-spends the given UTxO.
mkPaintTx ::
  -- | UTxO to spend
  (TxIn, TxOut CtxUTxO) ->
  -- | Signing key which owns the UTxO.
  SigningKey PaymentKey ->
  Pixel ->
  Either TxBodyError Tx
mkPaintTx (txin, txOut) sk Pixel{x, y, red, green, blue} = do
  body <- createAndValidateTransactionBody bodyContent
  pure $ signShelleyTransaction body [WitnessPaymentKey sk]
 where
  bodyContent =
    defaultTxBodyContent
      & addTxIn (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)
      & addTxOut (toTxContext txOut)
      & setTxFee (TxFeeExplicit $ Coin 0)
      & setTxMetadata metadata

  metadata = TxMetadataInEra $ TxMetadata $ fromList [(14, listOfInts)]

  listOfInts = TxMetaList $ TxMetaNumber . fromIntegral <$> [x, y, red, green, blue]
