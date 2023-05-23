{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import Hydra.Ledger (IsTx, UTxOType)

newtype DraftCommitTxResponse tx = DraftCommitTxResponse
  { commitTx :: tx
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (DraftCommitTxResponse tx)
deriving stock instance IsTx tx => Show (DraftCommitTxResponse tx)

instance (IsTx tx, ToCBOR tx) => ToJSON (DraftCommitTxResponse tx) where
  toJSON (DraftCommitTxResponse tx) =
    object
      [ "tag" .= String "DraftCommitTxResponse"
      , "commitTx" .= (String . decodeUtf8 . Base16.encode $ serialize' tx)
      ]

instance
  (IsTx tx, FromCBOR tx) =>
  FromJSON (DraftCommitTxResponse tx)
  where
  parseJSON = withObject "DraftCommitTxResponse" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "DraftCommitTxResponse" -> do
        encodedTx :: Text <- o .: "commitTx"
        case Base16.decode $ encodeUtf8 encodedTx of
          Left e -> fail e
          Right commitTx ->
            case decodeFull' commitTx of
              Left err -> fail $ show err
              Right v -> pure $ DraftCommitTxResponse v
      _ -> fail "Expected tag to be PubKeyHash"

instance IsTx tx => Arbitrary (DraftCommitTxResponse tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxResponse xs -> DraftCommitTxResponse <$> shrink xs

newtype DraftCommitTxRequest tx = DraftCommitTxRequest
  { utxo :: UTxOType tx
  }
  deriving (Generic)

deriving newtype instance IsTx tx => Eq (DraftCommitTxRequest tx)
deriving newtype instance IsTx tx => Show (DraftCommitTxRequest tx)
deriving anyclass instance IsTx tx => ToJSON (DraftCommitTxRequest tx)
deriving anyclass instance IsTx tx => FromJSON (DraftCommitTxRequest tx)

instance Arbitrary (UTxOType tx) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest xs -> DraftCommitTxRequest <$> shrink xs
