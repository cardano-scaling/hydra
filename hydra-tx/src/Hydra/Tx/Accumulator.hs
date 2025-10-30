{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  -- * Types
  HydraAccumulator (..),

  -- * Pure operations (for normal snapshots)
  makeHeadAccumulator,
  getAccumulatorHash,

  -- * IO operations (for partial fanout only)
  createMembershipProof,

  -- * Internal
  build,
  utxoToElement,
) where

import Hydra.Prelude

import Accumulator (Accumulator)
import Accumulator qualified
import Bindings (getPolyCommitOverG2)
import Cardano.Crypto.EllipticCurve.BLS12_381 (Point2)
import Codec.Serialise (serialise)
import Data.Aeson.Types (Parser)
import Data.ByteString.Base16 qualified as Base16
import Data.Map qualified as Map
import Data.Text.Encoding qualified as T
import Hydra.Cardano.Api (
  CtxUTxO,
  TxIn,
  TxOut,
  toPlutusTxOut,
  toPlutusTxOutRef,
 )
import Hydra.Tx.IsTx (IsTx (..))
import PlutusTx (toData)

-- * HydraAccumulator

newtype HydraAccumulator = HydraAccumulator {unHydraAccumulator :: Accumulator}
  deriving newtype (Eq, Show)

build :: [ByteString] -> HydraAccumulator
build = HydraAccumulator . Accumulator.buildAccumulator

instance ToJSON HydraAccumulator where
  toJSON (HydraAccumulator acc) =
    toJSON
      . Map.mapKeys (T.decodeUtf8 . Base16.encode)
      . fmap (first (T.decodeUtf8 . Base16.encode))
      $ acc

instance FromJSON HydraAccumulator where
  parseJSON value =
    parseJSON value >>= \(m :: Map Text (Text, Int)) ->
      let textToBs :: Text -> Parser ByteString
          textToBs t =
            case Base16.decode (T.encodeUtf8 t) of
              Left e -> fail $ "invalid base16: " <> e
              Right bs -> pure bs
       in HydraAccumulator
            . Map.fromList
            <$> forM
              (Map.toList m)
              ( \(k, (v, i)) -> do
                  k' <- textToBs k
                  v' <- textToBs v
                  pure (k', (v', i))
              )

-- * Accumulator functions

-- | Create a 'HydraAccumulator' from a UTxO set.
--
-- This is a pure function that builds the accumulator data structure.
makeHeadAccumulator :: forall tx. IsTx tx => UTxOType tx -> HydraAccumulator
makeHeadAccumulator utxo =
  let elements = utxoToElement <$> toPairList utxo
   in build elements

-- | Get a simple hash of the accumulator state.
--
-- This is a pure function that returns a deterministic hash of the accumulator's contents.
-- For off-chain snapshots, we just need a commitment to the UTxO set, not a cryptographic proof.
-- This hash is what gets signed by all parties in the multi-signature.
getAccumulatorHash :: HydraAccumulator -> ByteString
getAccumulatorHash (HydraAccumulator acc) =
  -- Simple serialization-based hash of the accumulator map
  toStrict . serialise $ acc

-- * Cryptographic Proofs (IO functions for partial fanout)

-- | Create a membership proof for a subset of UTxO elements.
--
-- This function is ONLY needed for partial fanout, where we need to prove that
-- a subset of UTxOs were actually in the confirmed snapshot.
--
-- NOTE: This requires IO because it performs elliptic curve cryptography operations.
createMembershipProof ::
  forall tx.
  IsTx tx =>
  -- | The subset of UTxO to prove membership of (e.g., the UTxOs being fanned out)
  UTxOType tx ->
  -- | The full accumulator from the confirmed snapshot
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point2] ->
  -- | Either an error message or the membership proof
  IO (Either String Point2)
createMembershipProof partialUTxO (HydraAccumulator fullAcc) crs = do
  -- Convert the partial UTxO to accumulator elements
  let partialElements = utxoToElement <$> toPairList partialUTxO
  -- Generate the cryptographic proof using the Bindings module
  getPolyCommitOverG2 partialElements fullAcc crs

-- | The canonical way to serialize a UTxO element for the accumulator.
--
-- The serialization is based on the Plutus `ToData` instances for a `TxOutRef`
-- and a `TxOut`.
utxoToElement :: (TxIn, TxOut CtxUTxO) -> ByteString
utxoToElement (txIn, txOut) =
  toStrict (serialise $ toData $ toPlutusTxOutRef txIn) <> toStrict (serialise $ toData $ toPlutusTxOut txOut)
