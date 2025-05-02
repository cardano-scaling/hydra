module Cardano.Api.Orphans.NetworkId where

import Cardano.Api (NetworkId (..))
import Cardano.Api.Orphans.NetworkMagic ()
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Text
import Test.Gen.Cardano.Api.Typed (genNetworkId)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Hedgehog (hedgehog)

-- * Orphans

instance ToJSON NetworkId where
  toJSON = \case
    Mainnet -> object ["tag" .= String "Mainnet"]
    Testnet magic ->
      object
        [ "tag" .= String "Testnet"
        , "magic" .= toJSON magic
        ]

instance FromJSON NetworkId where
  parseJSON = withObject "NetworkId" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "Mainnet" -> pure Mainnet
      "Testnet" -> Testnet <$> o .: "magic"
      _ -> fail "Expected tag to be Mainnet | Testnet"

instance Arbitrary NetworkId where
  arbitrary = hedgehog genNetworkId
  shrink = \case
    Mainnet -> []
    Testnet magic -> Testnet <$> shrink magic
