
module Hydra.API.APIServerLog where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import Data.ByteString.Short ()
import Hydra.Chain.Direct.State ()
import Hydra.Ledger.Cardano ()
import Hydra.Network (PortNumber)
import Test.QuickCheck (oneof)

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIOutputSent {sentOutput :: Aeson.Value}
  | APIInputReceived {receivedInput :: Aeson.Value}
  | APIInvalidInput {reason :: String, inputReceived :: Text}
  | APIConnectionError {reason :: String}
  | APIHandshakeError {reason :: String}
  | APIRestInputReceived
      { method :: Text
      , paths :: [Text]
      , requestInputBody :: Maybe Aeson.Value
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
      , APIInvalidInput <$> arbitrary <*> arbitrary
      , APIConnectionError <$> arbitrary
      , APIHandshakeError <$> arbitrary
      , APIRestInputReceived
          <$> arbitrary
          <*> arbitrary
          <*> oneof [pure Nothing, pure $ Just (Aeson.Object mempty)]
      ]

