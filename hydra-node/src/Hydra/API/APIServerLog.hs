module Hydra.API.APIServerLog where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Hydra.CBOR.Orphans ()
import Hydra.Network (PortNumber)

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIOutputSent {sentOutput :: Aeson.Value}
  | APIInputReceived {receivedInput :: Aeson.Value}
  | APIInvalidInput {reason :: String, inputReceived :: Text}
  | APIConnectionError {reason :: String}
  | APIHTTPRequestReceived
      { method :: Method
      , path :: PathInfo
      }
  | APITransactionSubmitted {submittedTxId :: String}
  | APIReturnedError {reason :: String}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance ToCBOR APIServerLog where
  toCBOR = \case
    APIServerStarted{listeningPort} ->
      toCBOR ("APIServerStarted" :: Text) <> toCBOR listeningPort
    NewAPIConnection ->
      toCBOR ("NewAPIConnection" :: Text)
    APIOutputSent{sentOutput} ->
      toCBOR ("APIOutputSent" :: Text) <> toCBOR sentOutput
    APIInputReceived{receivedInput} ->
      toCBOR ("APIInputReceived" :: Text) <> toCBOR receivedInput
    APIInvalidInput{reason, inputReceived} ->
      toCBOR ("APIInvalidInput" :: Text) <> toCBOR (toText reason) <> toCBOR inputReceived
    APIConnectionError{reason} ->
      toCBOR ("APIConnectionError" :: Text) <> toCBOR (toText reason)
    APIHTTPRequestReceived{method, path} ->
      toCBOR ("APIHTTPRequestReceived" :: Text) <> toCBOR method <> toCBOR path
    APITransactionSubmitted{submittedTxId} ->
      toCBOR ("APITransactionSubmitted" :: Text) <> toCBOR (toText submittedTxId)
    APIReturnedError{reason} ->
      toCBOR ("APIReturnedError" :: Text) <> toCBOR (toText reason)

instance FromCBOR APIServerLog where
  fromCBOR =
    fromCBOR >>= \case
      ("APIServerStarted" :: Text) -> APIServerStarted <$> fromCBOR
      "NewAPIConnection" -> pure NewAPIConnection
      "APIOutputSent" -> APIOutputSent <$> fromCBOR
      "APIInputReceived" -> APIInputReceived <$> fromCBOR
      "APIInvalidInput" -> APIInvalidInput <$> (toString <$> fromCBOR @Text) <*> fromCBOR
      "APIConnectionError" -> APIConnectionError . toString <$> fromCBOR @Text
      "APIHTTPRequestReceived" -> APIHTTPRequestReceived <$> fromCBOR <*> fromCBOR
      "APITransactionSubmitted" -> APITransactionSubmitted . toString <$> fromCBOR @Text
      "APIReturnedError" -> APIReturnedError . toString <$> fromCBOR @Text
      tag -> fail $ show tag <> " is not a proper CBOR-encoded APIServerLog"

-- | New type wrapper to define JSON instances.
newtype PathInfo = PathInfo ByteString
  deriving stock (Eq, Show)

instance ToJSON PathInfo where
  toJSON (PathInfo bytes) =
    Aeson.String $ decodeUtf8 bytes

instance ToCBOR PathInfo where
  toCBOR (PathInfo bytes) = toCBOR bytes

instance FromCBOR PathInfo where
  fromCBOR = PathInfo <$> fromCBOR

-- | New type wrapper to define JSON instances.
--
-- NOTE: We are not using http-types 'StdMethod' as we do not want to be
-- constrained in terms of logging and accept any method in a 'Request'.
newtype Method = Method ByteString
  deriving stock (Eq, Show)

instance ToJSON Method where
  toJSON (Method bytes) =
    Aeson.String $ decodeUtf8 bytes

instance ToCBOR Method where
  toCBOR (Method bytes) = toCBOR bytes

instance FromCBOR Method where
  fromCBOR = Method <$> fromCBOR
