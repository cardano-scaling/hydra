{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Short ()
import Hydra.Cardano.Api (HashableScriptData, PlutusScript, TxIn, TxOut, CtxUTxO)
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
      [ "commitTx" .= (String . decodeUtf8 . Base16.encode $ serialize' tx)
      ]

instance
  (IsTx tx, FromCBOR tx) =>
  FromJSON (DraftCommitTxResponse tx)
  where
  parseJSON = withObject "DraftCommitTxResponse" $ \o -> do
    encodedTx :: Text <- o .: "commitTx"
    case Base16.decode $ encodeUtf8 encodedTx of
      Left e -> fail e
      Right commitTx ->
        case decodeFull' commitTx of
          Left err -> fail $ show err
          Right v -> pure $ DraftCommitTxResponse v

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

data DraftUTxO tx = DraftUTxO
  { draftTxIn :: TxIn
  , draftTxOut :: TxOut CtxUTxO
  , draftScriptInfo :: Maybe ScriptInfo
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (DraftUTxO tx)
deriving stock instance IsTx tx => Show (DraftUTxO tx)
deriving anyclass instance IsTx tx => ToJSON (DraftUTxO tx)
deriving anyclass instance IsTx tx => FromJSON (DraftUTxO tx)

instance Arbitrary (UTxOType tx) => Arbitrary (DraftUTxO tx) where
  arbitrary = genericArbitrary

newtype DraftCommitTxRequest tx = DraftCommitTxRequest
  { utxos :: [DraftUTxO tx]
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (DraftCommitTxRequest tx)
deriving stock instance IsTx tx => Show (DraftCommitTxRequest tx)

instance (IsTx tx, ToCBOR tx) => ToJSON (DraftCommitTxRequest tx) where
  toJSON (DraftCommitTxRequest utxo) =
    object
      [ "utxos" .= toJSON utxo
      ]

instance
  (IsTx tx, FromCBOR tx) =>
  FromJSON (DraftCommitTxRequest tx)
  where
  parseJSON = withObject "DraftCommitTxRequest" $ \o -> do
    utxos :: [DraftUTxO tx] <- o .: "utxos"
    pure $ DraftCommitTxRequest utxos

instance Arbitrary (UTxOType tx) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest u -> DraftCommitTxRequest <$> shrink u
