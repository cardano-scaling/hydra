{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

import Data.Aeson (Value (String), object, withObject, (.:), (.=))

-- * Orphans

instance ToJSON ChainPoint where
  toJSON = \case
    ChainPointAtGenesis -> object ["tag" .= String "ChainPointAtGenesis"]
    ChainPoint slot blockHash ->
      object
        [ "tag" .= String "ChainPoint"
        , "slot" .= toJSON slot
        , "blockHash" .= toJSON blockHash
        ]

instance FromJSON ChainPoint where
  parseJSON = withObject "NetworkId" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "ChainPointAtGenesis" -> pure ChainPointAtGenesis
      "ChainPoint" -> ChainPoint <$> o .: "slot" <*> o .: "blockHash"
      _ -> fail "Expected tag to be ChainPointAtGenesis | ChainPoint"
