{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import Hydra.Ledger (IsTx, UTxOType)

newtype RestServerOutput tx = DraftedCommitTx
  { commitTx :: tx
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (RestServerOutput tx)
deriving stock instance IsTx tx => Show (RestServerOutput tx)

instance (IsTx tx, ToCBOR tx) => ToJSON (RestServerOutput tx) where
  toJSON (DraftedCommitTx tx) =
    object
      [ "tag" .= String "RestServerOutput"
      , "commitTx" .= (String . decodeUtf8 . Base16.encode $ serialize' tx)
      ]

instance
  (IsTx tx, FromCBOR tx) =>
  FromJSON (RestServerOutput tx)
  where
  parseJSON = withObject "RestServerOutput" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "RestServerOutput" -> do
        encodedTx :: Text <- o .: "commitTx"
        case Base16.decode $ encodeUtf8 encodedTx of
          Left e -> fail e
          Right commitTx ->
            case decodeFull' commitTx of
              Left err -> fail $ show err
              Right v -> pure $ DraftedCommitTx v
      _ -> fail "Expected tag to be PubKeyHash"

instance IsTx tx => Arbitrary (RestServerOutput tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftedCommitTx xs -> DraftedCommitTx <$> shrink xs

newtype RestClientInput tx = DraftCommitTx
  { utxo :: UTxOType tx
  }
  deriving (Generic)

deriving newtype instance IsTx tx => Eq (RestClientInput tx)
deriving newtype instance IsTx tx => Show (RestClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (RestClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (RestClientInput tx)

instance Arbitrary (UTxOType tx) => Arbitrary (RestClientInput tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTx xs -> DraftCommitTx <$> shrink xs
