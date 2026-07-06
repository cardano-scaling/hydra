{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.NetworkId where

import Hydra.Cardano.Api.Prelude

import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import Hydra.Cardano.Api.NetworkMagic ()

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

-- missing CBOR instances

instance ToCBOR NetworkId where
  toCBOR = \case
    Mainnet -> toCBOR ("Mainnet" :: Text)
    Testnet (NetworkMagic magic) -> toCBOR ("Testnet" :: Text) <> toCBOR magic

instance FromCBOR NetworkId where
  fromCBOR =
    fromCBOR >>= \case
      ("Mainnet" :: Text) -> pure Mainnet
      "Testnet" -> Testnet . NetworkMagic <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded NetworkId"
