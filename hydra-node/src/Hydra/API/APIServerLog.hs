module Hydra.API.APIServerLog where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Hydra.Network (PortNumber)
import Network.HTTP.Types (renderStdMethod)
import Test.QuickCheck (chooseEnum, listOf, oneof)

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary APIServerLog where
  arbitrary =
    oneof
      [ APIServerStarted <$> arbitrary
      , pure NewAPIConnection
      , pure $ APIOutputSent (Aeson.Object mempty)
      , pure $ APIInputReceived (Aeson.Object mempty)
      , APIInvalidInput <$> arbitrary <*> (Text.pack <$> listOf arbitrary)
      , APIConnectionError <$> arbitrary
      , APIHTTPRequestReceived <$> arbitrary <*> arbitrary
      ]

-- | New type wrapper to define JSON instances.
newtype PathInfo = PathInfo ByteString
  deriving (Eq, Show)

instance Arbitrary PathInfo where
  arbitrary =
    PathInfo . encodeUtf8 . Text.pack <$> listOf arbitrary

instance ToJSON PathInfo where
  toJSON (PathInfo bytes) =
    Aeson.String $ decodeUtf8 bytes

instance FromJSON PathInfo where
  parseJSON = Aeson.withText "PathInfo" $ \t ->
    pure . PathInfo $ encodeUtf8 t

-- | New type wrapper to define JSON instances.
--
-- NOTE: We are not using http-types 'StdMethod' as we do not want to be
-- constrained in terms of logging and accept any method in a 'Request'.
newtype Method = Method ByteString
  deriving (Eq, Show)

instance Arbitrary Method where
  arbitrary = Method . renderStdMethod <$> chooseEnum (minBound, maxBound)

instance ToJSON Method where
  toJSON (Method bytes) =
    Aeson.String $ decodeUtf8 bytes

instance FromJSON Method where
  parseJSON = Aeson.withText "Method" $ \t ->
    pure . Method $ encodeUtf8 t
