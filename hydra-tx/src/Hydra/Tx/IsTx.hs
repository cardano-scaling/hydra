{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- NOTE: For serialiseTxLedgerCddl
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.IsTx where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Cardano.Api.Tx qualified as Api
import Hydra.Cardano.Api.UTxO qualified as Api
import Hydra.Contract.Util qualified as Util
import "aeson" Data.Aeson ((.:), (.:?))
import "aeson" Data.Aeson qualified as Aeson
import "aeson" Data.Aeson.KeyMap qualified as KeyMap
import "aeson" Data.Aeson.Types (withObject)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-binary" Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import "cardano-ledger-shelley" Cardano.Ledger.Shelley.UTxO qualified as Ledger
import "cborg" Codec.CBOR.Decoding qualified as CBOR
import "cborg" Codec.CBOR.Encoding qualified as CBOR
import "formatting" Formatting.Buildable (build)
import "plutus-ledger-api" PlutusLedgerApi.V3 (fromBuiltin)
import "text" Data.Text.Lazy.Builder (toLazyText)

-- | Types of transactions that can be used by the Head protocol. The associated
-- types and methods of this type class represent the whole interface of what
-- the Head protocol needs from a transaction. This ensure the off-chain
-- protocol stays fairly independent of a concrete transaction type.
class
  ( Eq tx
  , Show tx
  , Typeable tx
  , FromCBOR tx
  , ToCBOR tx
  , FromJSON tx
  , ToJSON tx
  , --
    Eq (TxIdType tx)
  , Ord (TxIdType tx)
  , Show (TxIdType tx)
  , Typeable (TxIdType tx)
  , FromJSON (TxIdType tx)
  , ToJSON (TxIdType tx)
  , FromCBOR (TxIdType tx)
  , ToCBOR (TxIdType tx)
  , FromJSONKey (TxIdType tx)
  , ToJSONKey (TxIdType tx)
  , --
    Eq (TxOutType tx)
  , Show (TxOutType tx)
  , ToJSON (TxOutType tx)
  , FromJSON (TxOutType tx)
  , --
    Eq (UTxOType tx)
  , Show (UTxOType tx)
  , Monoid (UTxOType tx)
  , FromJSON (UTxOType tx)
  , ToJSON (UTxOType tx)
  , FromCBOR (UTxOType tx)
  , ToCBOR (UTxOType tx)
  ) =>
  IsTx tx
  where
  -- | Type which identifies a transaction
  type TxIdType tx

  -- | Type for individual transaction outputs.
  type TxOutType tx = out | out -> tx

  -- | Type for a set of unspent transaction outputs.
  type UTxOType tx = utxo | utxo -> tx

  -- | Type representing a value on the ledger.
  type ValueType tx

  -- XXX(SN): this name easily conflicts
  txId :: tx -> TxIdType tx

  -- XXX: Is this even used?
  balance :: UTxOType tx -> ValueType tx

  -- | Hash a utxo set to be able to sign (off-chain) and verify it (on-chain).
  hashUTxO :: UTxOType tx -> ByteString

  txSpendingUTxO :: UTxOType tx -> tx

  -- | Get the UTxO produced by given transaction.
  utxoFromTx :: tx -> UTxOType tx

  -- | Get only the outputs in given UTxO.
  outputsOfUTxO :: UTxOType tx -> [TxOutType tx]

  -- | Return the left-hand side without the right-hand side.
  withoutUTxO :: UTxOType tx -> UTxOType tx -> UTxOType tx

-- * Cardano Tx

instance IsShelleyBasedEra era => ToJSON (Api.Tx era) where
  toJSON tx =
    -- XXX: This is a deprecated function, but the only one that produces the
    -- right 'Tx ConwayEra' in the envelope type. Cardano-api will be
    -- fixing the 'HasTextEnvelope' instance for 'Tx era' and then we can use
    -- 'serialiseToTextEnvelope' here.
    case toJSON $ serialiseToTextEnvelope Nothing tx of
      Aeson.Object km ->
        Aeson.Object $ KeyMap.insert "txId" (toJSON $ getTxId $ getTxBody tx) km
      v -> v

instance FromJSON Tx where
  parseJSON =
    withObject "Tx" $ \o -> do
      hexText <- o .: "cborHex"
      -- NOTE: We deliberately ignore the "type" to be backwards compatible
      bytes <- decodeBase16 hexText
      case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
        Left e -> fail $ show e
        Right tx -> do
          -- NOTE: Check txId equivalence only if present.
          (o .:? "txId") >>= \case
            Just txid'
              | txid' /= Hydra.Tx.IsTx.txId tx -> fail "txId not matching"
            _ -> pure tx

-- XXX: Double CBOR encoding?
instance IsShelleyBasedEra era => ToCBOR (Api.Tx era) where
  toCBOR = CBOR.encodeBytes . serialiseToCBOR

-- XXX: Double CBOR encoding?
instance FromCBOR Tx where
  fromCBOR = do
    bs <- CBOR.decodeBytes
    decodeFullAnnotator ledgerEraVersion "Tx" decCBOR (fromStrict bs)
      & either
        (fail . toString . toLazyText . build)
        (pure . fromLedgerTx)

instance ToCBOR UTxO where
  toCBOR = toCBOR . UTxO.toShelleyUTxO shelleyBasedEra
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR UTxO where
  fromCBOR = UTxO.fromShelleyUTxO shelleyBasedEra <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

instance IsTx Tx where
  type TxIdType Tx = TxId
  type TxOutType Tx = TxOut CtxUTxO
  type UTxOType Tx = UTxO
  type ValueType Tx = Value

  txId = getTxId . getTxBody
  balance = UTxO.totalValue

  -- NOTE: See note from `Util.hashTxOuts`.
  hashUTxO = fromBuiltin . Util.hashTxOuts . mapMaybe toPlutusTxOut . UTxO.txOutputs

  txSpendingUTxO = Api.txSpendingUTxO

  utxoFromTx = Api.utxoFromTx

  outputsOfUTxO = UTxO.txOutputs

  withoutUTxO = UTxO.difference
