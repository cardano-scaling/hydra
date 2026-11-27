{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.IsTx (
  module Hydra.Tx.IsTx.Core,
) where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (withObject)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)
import Hydra.Cardano.Api (CtxUTxO, IsShelleyBasedEra, LedgerEra, Tx, TxId, TxOut, UTxO, Value, deserialiseFromCBOR, fromLedgerTx, getTxBody, getTxId, ledgerEraVersion, parseUTxOFromJSON, proxyToAsType, serialiseToCBOR, serialiseToRawBytes, serialiseToTextEnvelope, shelleyBasedEra, toPlutusTxOut, pattern UTxO)
import Hydra.Cardano.Api qualified as Cardano
import Hydra.Cardano.Api.Tx qualified as Api
import Hydra.Cardano.Api.UTxO qualified as Api
import Hydra.Contract.Util qualified as Util
import Hydra.Tx.IsTx.Core
import PlutusLedgerApi.V3 (fromBuiltin)

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

-- | Orphan 'Ord' instance for 'TxOut CtxUTxO' using JSON encoding for comparison.
-- Needed to use 'TxOut CtxUTxO' in 'Set' and as 'TxOutType Tx' with the 'Ord' constraint.
--
-- XXX: This fully serialises both operands on every comparison (including any
-- reference script, as hex CBOR), so it is O(size of output) rather than O(1)
-- and allocates on each call. It is kept as-is deliberately: the ordering it
-- induces is observable through every 'Set'/'Map' keyed by an output, so a
-- cheaper key (e.g. comparing CBOR instead) would silently reorder them.
-- cardano-ledger provides no 'Ord' for 'BabbageTxOut' to delegate to, so
-- changing this needs a deliberate decision about the new ordering rather than
-- a drop-in replacement.
instance Ord (TxOut CtxUTxO) where
  compare x y = compare (Aeson.encode x) (Aeson.encode y)

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
              | txid' /= txId tx -> fail "txId not matching"
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
  -- NOTE: Use the forcing conversion to uphold the invariant that any 'UTxO'
  -- entering the head logic is thunk-free.
  fromCBOR = Api.fromLedgerUTxO <$> fromCBOR
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

  -- NOTE: Same 32 bytes as the on-chain 'getTxId' of a 'TxOutRef'.
  txIdBytes = serialiseToRawBytes

  txSpendingUTxO = Api.txSpendingUTxO

  utxoFromTx = Api.utxoFromTx

  outputsOfUTxO = UTxO.txOutputs

  withoutUTxO = UTxO.difference

  txInputsAvailable tx (UTxO available) = all (`Map.member` available) required
   where
    body = Cardano.getTxBodyContent $ getTxBody tx
    required = (fst <$> Cardano.txIns body) <> references <> collateral
    references = case Cardano.txInsReference body of
      Cardano.TxInsReferenceNone -> []
      Cardano.TxInsReference inputs _ -> inputs
    collateral = case Cardano.txInsCollateral body of
      Cardano.TxInsCollateralNone -> []
      Cardano.TxInsCollateral inputs -> inputs

  withoutTxInputs tx utxo = utxo `UTxO.difference` Api.resolveInputsUTxO utxo tx

  applyTxTo tx utxo = withoutTxInputs tx utxo <> Api.utxoFromTx tx

  filterUTxOByOutputs utxo outputs = UTxO.filter (`Set.member` outputs) utxo

  removeOneOutputFromUTxO out (UTxO m) =
    case Map.toAscList (Map.filter (== out) m) of
      [] -> UTxO m
      (txin, _) : _ -> UTxO (Map.delete txin m)

  -- \| Convert a Cardano UTxO pair to a ByteString element using Plutus serialization.
  -- Uses sha2_256 (via hashTxOuts) because the haskell-accumulator library
  -- internally applies Blake2b_224 on each element (see Accumulator.addElement).
  -- The on-chain scalar is then blake2b_224(sha2_256(serialised)), matching exactly.
  utxoToElement txOut =
    case toPlutusTxOut txOut of
      Just plutusTxOut -> fromBuiltin (Util.hashTxOuts [plutusTxOut])
      Nothing -> mempty -- Should not happen for valid UTxO

instance {-# OVERLAPPING #-} FromJSON UTxO where
  parseJSON = parseUTxOFromJSON
