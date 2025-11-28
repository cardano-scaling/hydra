module Hydra.Painter where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Control.Exception (IOException)
import Data.Aeson qualified as Aeson
import Data.Text (unpack)
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.Chain.Direct.State ()
import Hydra.Network (Host (..))
import Hydra.Node.Util (readFileTextEnvelopeThrow)
import Network.HTTP.Conduit (parseUrlThrow)
import Network.HTTP.Simple (getResponseBody, httpJSON)
import Network.WebSockets (Connection, runClient, sendTextData)
import Network.WebSockets.Connection (receive)

data Pixel = Pixel
  { x, y, red, green, blue :: Word8
  }

paintPixel :: NetworkId -> FilePath -> Host -> Connection -> Pixel -> IO ()
paintPixel networkId signingKeyPath host cnx pixel = do
  sk <- readFileTextEnvelopeThrow signingKeyPath
  let myAddress = mkVkAddress networkId $ getVerificationKey sk
  flushQueue

  putStrLn $ "About to request head UTxO"

  mHeadUTxO <- requestHeadUTxO host
    `catch` \(e :: SomeException) -> do
      -- XXX: Fixme
      print e
      putStrLn "Error requesting Head UTxO"
      error "error requesting Head UTxO"

  case mHeadUTxO of
    Just utxo ->
      case UTxO.find (\TxOut{txOutAddress} -> txOutAddress == myAddress) utxo of
        Nothing -> fail $ "No UTxO owned by " <> show myAddress
        Just (txIn, txOut) ->
          case mkPaintTx (txIn, txOut) sk pixel of
            Right tx -> do
              sendTextData cnx $ Aeson.encode $ NewTx tx
            Left err -> fail $ "Failed to build pixel transaction " <> show err
    Nothing -> fail "Head UTxO is empty"
 where
  flushQueue =
    raceLabelled_ ("thread-delay", threadDelay 0.25) ("flush-queue", void (receive cnx) >> flushQueue)

requestHeadUTxO :: Host -> IO (Maybe UTxO)
requestHeadUTxO host = do
  resp <-
    parseUrlThrow ("GET " <> unpack (hydraNodeBaseUrl host) <> "/snapshot/utxo")
      >>= httpJSON
  pure $ getResponseBody resp
 where
  hydraNodeBaseUrl Host{hostname, port} = "http://" <> hostname <> ":" <> show port

-- | Same as 'withClient' except we don't retry if connection fails.
withClientNoRetry :: Bool -> Host -> (Connection -> IO ()) -> IO ()
withClientNoRetry b Host{hostname, port} action = do
  -- putStrLn $ "Host= " <> show hostname
  -- putStrLn $ "Port= " <> show port
  runClient (unpack hostname) (fromIntegral port) (url b) action
    `catch` \(e :: IOException) -> do
      putStrLn "Couldn't open websocket client connection"
      putStrLn "Reason: "
      print e
      threadDelay 1
  where
      -- FIXME: Do this nicer
      url :: Bool -> String
      url True  = "/?history=yes"
      url False = "/"

withClient :: Host -> (Connection -> IO ()) -> IO ()
withClient Host{hostname, port} action =
  retry
 where
  retry = do
    putTextLn $ "Connecting to Hydra API on " <> hostname <> ":" <> show port <> ".."
    runClient (toString hostname) (fromIntegral port) "/?history=yes" action
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
      & addTxOut (fromCtxUTxOTxOut txOut)
      & setTxFee (TxFeeExplicit $ Coin 0)
      & setTxMetadata metadata

  metadata = TxMetadataInEra $ TxMetadata $ fromList [(14, listOfInts)]

  listOfInts = TxMetaList $ TxMetaNumber . fromIntegral <$> [x, y, red, green, blue]
