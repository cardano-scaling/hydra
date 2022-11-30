{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

import Data.Aeson (Value (String), object, withObject, (.:), (.=))

-- * Orphans

-- NOTE: convenient orphan to compare points
instance Ord ChainPoint where
  compare ChainPointAtGenesis ChainPointAtGenesis = EQ
  compare ChainPointAtGenesis _ = LT
  compare _ ChainPointAtGenesis = GT
  compare (ChainPoint sn _) (ChainPoint sn' _) = compare sn sn'

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
  parseJSON = withObject "ChainPoint" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "ChainPointAtGenesis" -> pure ChainPointAtGenesis
      "ChainPoint" -> ChainPoint <$> o .: "slot" <*> o .: "blockHash"
      _ -> fail "Expected tag to be ChainPointAtGenesis | ChainPoint"

-- XXX: Incomplete arbitrary instance
instance Arbitrary ChainPoint where
  arbitrary =
    pure ChainPointAtGenesis
