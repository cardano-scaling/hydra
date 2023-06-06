{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Short ()
import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import Hydra.Cardano.Api.Prelude (
  HashableScriptData,
 )
import Hydra.Ledger (IsTx, UTxOType)
import Hydra.Ledger.Cardano ()

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
      _ -> fail "Expected tag to be DraftCommitTxResponse"

instance IsTx tx => Arbitrary (DraftCommitTxResponse tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxResponse xs -> DraftCommitTxResponse <$> shrink xs

data ScriptInfo = ScriptInfo
  { redeemer :: HashableScriptData
  , datum :: HashableScriptData
  , script :: PlutusScript
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ScriptInfo where
  arbitrary = genericArbitrary

data DraftCommitTxRequest tx = DraftCommitTxRequest
  { utxo :: UTxOType tx
  , scriptInfo :: Maybe ScriptInfo
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (DraftCommitTxRequest tx)
deriving stock instance IsTx tx => Show (DraftCommitTxRequest tx)

instance (IsTx tx, ToCBOR tx) => ToJSON (DraftCommitTxRequest tx) where
  toJSON (DraftCommitTxRequest utxo scriptInfo) =
    object
      [ "tag" .= String "DraftCommitTxRequest"
      , "utxos" .= toJSON utxo
      , "scriptInfo" .= toJSON scriptInfo
      ]

instance
  (IsTx tx, FromCBOR tx) =>
  FromJSON (DraftCommitTxRequest tx)
  where
  parseJSON = withObject "DraftCommitTxRequest" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "DraftCommitTxRequest" -> do
        utxos :: (UTxOType tx) <- o .: "utxos"
        scriptInfo :: (Maybe ScriptInfo) <- o .: "scriptInfo"
        pure $ DraftCommitTxRequest utxos scriptInfo
      _ -> fail "Expected tag to be DraftCommitTxRequest"

instance Arbitrary (UTxOType tx) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest xs si -> DraftCommitTxRequest <$> shrink xs <*> shrink si

-- * Orphans
instance Arbitrary PlutusScript where
  arbitrary = do
    word8 :: ShortByteString <- arbitrary
    pure $ PlutusScriptSerialised word8
