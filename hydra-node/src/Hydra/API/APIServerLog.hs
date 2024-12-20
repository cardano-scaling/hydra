module Hydra.API.APIServerLog where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
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
  deriving anyclass (ToJSON)

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

  shrink = \case
    APIInvalidInput r i -> [APIInvalidInput r' (Text.pack i') | r' <- shrink r, i' <- shrink (Text.unpack i)]
    _other -> []

-- | New type wrapper to define JSON instances.
newtype PathInfo = PathInfo ByteString
  deriving stock (Eq, Show)

instance Arbitrary PathInfo where
  arbitrary =
    PathInfo . encodeUtf8 . Text.pack <$> listOf arbitrary

instance ToJSON PathInfo where
  toJSON (PathInfo bytes) =
    Aeson.String $ decodeUtf8 bytes

-- | New type wrapper to define JSON instances.
--
-- NOTE: We are not using http-types 'StdMethod' as we do not want to be
-- constrained in terms of logging and accept any method in a 'Request'.
newtype Method = Method ByteString
  deriving stock (Eq, Show)

instance Arbitrary Method where
  arbitrary = Method . renderStdMethod <$> chooseEnum (minBound, maxBound)

instance ToJSON Method where
  toJSON (Method bytes) =
    Aeson.String $ decodeUtf8 bytes
