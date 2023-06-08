{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Short ()
import Hydra.Cardano.Api (CtxUTxO, HashableScriptData, PlutusScript, TxIn, TxOut)
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
      [ "tag" .= String "DraftCommitTxResponse" -- TODO: tag should be not needed
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

-- TODO: This should actually be isomorphic to ScriptWitness of cardano-api,
-- i.e. we should support also native scripts, other versions of plutus and
-- witnessing via reference inputs
data ScriptInfo = ScriptInfo
  { redeemer :: HashableScriptData
  , datum :: HashableScriptData
  , plutusV2Script :: PlutusScript
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ScriptInfo where
  arbitrary = genericArbitrary

data DraftCommitTxRequest tx = DraftCommitTxRequest
  { regularUtxo :: UTxOType tx
  , scriptUtxo :: [(TxIn, TxOut CtxUTxO, ScriptInfo)]
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (DraftCommitTxRequest tx)
deriving stock instance IsTx tx => Show (DraftCommitTxRequest tx)

instance (IsTx tx, ToCBOR tx) => ToJSON (DraftCommitTxRequest tx) where
  toJSON (DraftCommitTxRequest regularUtxo scriptUtxo) =
    object
      [ "tag" .= String "DraftCommitTxRequest" -- TODO: tag should be not needed
      , "regularUtxo" .= toJSON regularUtxo
      , "scriptUtxo" .= toJSON scriptUtxo
      ]

instance
  (IsTx tx, FromCBOR tx) =>
  FromJSON (DraftCommitTxRequest tx)
  where
  parseJSON = withObject "DraftCommitTxRequest" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "DraftCommitTxRequest" -> do
        regularUtxo :: (UTxOType tx) <- o .: "regularUtxo"
        scriptUtxo :: [(TxIn, TxOut CtxUTxO, ScriptInfo)] <- o .: "scriptUtxo"
        pure $ DraftCommitTxRequest regularUtxo scriptUtxo
      _ -> fail "Expected tag to be DraftCommitTxRequest"

instance Arbitrary (UTxOType tx) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest rUTxO sUTxO -> DraftCommitTxRequest <$> shrink rUTxO <*> shrink sUTxO
