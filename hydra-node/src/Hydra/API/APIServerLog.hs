module Hydra.API.APIServerLog where

import "hydra-prelude" Hydra.Prelude hiding (encodeUtf8)
import "aeson" Data.Aeson qualified as Aeson
import "text" Data.Text.Encoding (encodeUtf8)

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
  deriving anyclass (ToJSON, FromJSON)

-- | New type wrapper to define JSON instances.
newtype PathInfo = PathInfo ByteString
  deriving stock (Eq, Show)

instance ToJSON PathInfo where
  toJSON (PathInfo bytes) =
    Aeson.String $ decodeUtf8 bytes

instance FromJSON PathInfo where
  parseJSON = Aeson.withText "PathInfo" $ \t ->
    pure $ PathInfo $ encodeUtf8 t

-- | New type wrapper to define JSON instances.
--
-- NOTE: We are not using http-types 'StdMethod' as we do not want to be
-- constrained in terms of logging and accept any method in a 'Request'.
newtype Method = Method ByteString
  deriving stock (Eq, Show)

instance ToJSON Method where
  toJSON (Method bytes) =
    Aeson.String $ decodeUtf8 bytes

instance FromJSON Method where
  parseJSON = Aeson.withText "Method" $ \t ->
    pure $ Method $ encodeUtf8 t
