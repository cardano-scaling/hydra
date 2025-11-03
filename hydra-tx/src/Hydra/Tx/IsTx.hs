{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- NOTE: For serialiseTxLedgerCddl
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.IsTx where

import Hydra.Cardano.Api
import Hydra.Prelude

import Accumulator qualified
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.Serialise (serialise)
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (withObject)
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)
import Hydra.Cardano.Api.Tx qualified as Api
import Hydra.Cardano.Api.UTxO qualified as Api
import Hydra.Contract.Util qualified as Util
import PlutusLedgerApi.V3 (fromBuiltin, toBuiltinData, toData)
import PlutusTx.Builtins qualified as Builtins

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

  -- | Type for (input, output) pairs used in accumulator operations.
  type UTxOPairType tx = pair | pair -> tx

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

  -- | Convert a 'UTxOType' to a list of (input, output) pairs for accumulator operations.
  -- This is needed by the accumulator to convert UTxOs into elements.
  toPairList :: UTxOType tx -> [UTxOPairType tx]

  -- | Convert a UTxO pair to a ByteString element for the accumulator.
  utxoToElement :: UTxOPairType tx -> ByteString

-- * Constraint synonyms

type ArbitraryIsTx tx =
  ( IsTx tx
  , Arbitrary tx
  , Arbitrary (UTxOType tx)
  , Arbitrary (TxIdType tx)
  , Arbitrary (TxOutType tx)
  )

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
  type UTxOPairType Tx = (TxIn, TxOut CtxUTxO)

  txId = getTxId . getTxBody
  balance = UTxO.totalValue

  -- NOTE: See note from `Util.hashTxOuts`.
  -- NOTE: This uses accumulator-based hashing via toPairList and utxoToElement.
  hashUTxO utxo =
    let pairs = toPairList utxo
     in if null pairs
          then
            -- For empty UTxO, return the same hash as on-chain emptyHash = hashTxOuts [] = sha2_256 ""
            -- This is the SHA2-256 hash of an empty bytestring
            fromBuiltin $ Util.hashTxOuts []
          else
            let
              -- Build accumulator from UTxO pairs using utxoToElement
              elements = utxoToElement <$> pairs
              accumulator = Accumulator.buildAccumulator elements
              -- Serialize the accumulator and hash it with SHA2-256
              serializedAcc = toStrict (serialise accumulator)
             in
              fromBuiltin (Builtins.sha2_256 (Builtins.toBuiltin serializedAcc))

  txSpendingUTxO = Api.txSpendingUTxO

  utxoFromTx = Api.utxoFromTx

  outputsOfUTxO = UTxO.txOutputs

  withoutUTxO = UTxO.difference

  toPairList = UTxO.toList

  -- \| Convert a Cardano UTxO pair to a ByteString element using Plutus serialization
  utxoToElement (_txIn, txOut) =
    case toPlutusTxOut txOut of
      Just plutusTxOut ->
        fromBuiltin (Builtins.serialiseData (toBuiltinData plutusTxOut))
      Nothing -> mempty
