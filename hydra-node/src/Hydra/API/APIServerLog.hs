module Hydra.API.APIServerLog where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
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
  | APITransactionSubmitted {txid :: String}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | New type wrapper to define JSON instances.
newtype PathInfo = PathInfo ByteString
  deriving stock (Eq, Show)

instance ToJSON PathInfo where
  toJSON (PathInfo bytes) =
    Aeson.String $ decodeUtf8 bytes

-- | New type wrapper to define JSON instances.
--
-- NOTE: We are not using http-types 'StdMethod' as we do not want to be
-- constrained in terms of logging and accept any method in a 'Request'.
newtype Method = Method ByteString
  deriving stock (Eq, Show)

instance ToJSON Method where
  toJSON (Method bytes) =
    Aeson.String $ decodeUtf8 bytes
