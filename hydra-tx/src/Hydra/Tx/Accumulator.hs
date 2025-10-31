{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  HydraAccumulator (..),
  HasAccumulatorElement (..),
  makeHeadAccumulator,
  getAccumulatorHash,
  -- createMembershipProof,

  build,
  utxoToElement,
) where

import Hydra.Prelude

import Accumulator (Accumulator)
import Accumulator qualified

-- import Bindings (getPolyCommitOverG2)
import Cardano.Api.UTxO qualified as UTxO

-- import Cardano.Crypto.EllipticCurve.BLS12_381 (Point2)
import Codec.Serialise (serialise)
import Hydra.Cardano.Api (
  CtxUTxO,
  Tx,
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

-- | Create a 'HydraAccumulator' from a UTxO set.
makeHeadAccumulator :: forall tx. HasAccumulatorElement tx => UTxOType tx -> HydraAccumulator
makeHeadAccumulator utxo =
  let elements = utxoToElement @tx <$> toPairList utxo
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
-- This function is needed where we need to prove that
-- a subset of UTxOs were actually in the confirmed snapshot.
-- createMembershipProof ::
--   forall tx.
--   HasAccumulatorElement tx =>
--   -- | The subset of UTxO to prove membership of (e.g., the UTxOs being fanned out)
--   UTxOType tx ->
--   -- | The full accumulator from the confirmed snapshot
--   HydraAccumulator ->
--   -- | Common Reference String (CRS) for the cryptographic proof
--   [Point2] ->
--   -- | Either an error message or the membership proof
--   IO (Either String Point2)
-- createMembershipProof partialUTxO (HydraAccumulator fullAcc) crs = do
--   -- Convert the partial UTxO to accumulator elements
--   let partialElements = utxoToElement @tx <$> toPairList partialUTxO
--   -- Generate the cryptographic proof using the Bindings module
--   getPolyCommitOverG2 partialElements fullAcc crs

-- | Convert a UTxO pair to a ByteString element for the accumulator.
--
-- This is a polymorphic function that works for any transaction type implementing IsTx.
-- For Cardano transactions, it uses Plutus serialization.
-- For SimpleTx, it uses CBOR serialization.
utxoToElement :: forall tx. HasAccumulatorElement tx => UTxOPairType tx -> ByteString
utxoToElement = utxoToElementImpl @tx

-- | Type class for transactions that support accumulator operations.
--
-- This is separate from IsTx because not all transaction types need accumulator support.
-- For example, SimpleTx is only used for testing and doesn't need accumulator functionality.
class IsTx tx => HasAccumulatorElement tx where
  -- | Type for (input, output) pairs used in accumulator.
  type UTxOPairType tx

  -- | Convert a 'UTxOType' to a list of (input, output) pairs.
  toPairList :: UTxOType tx -> [UTxOPairType tx]

  -- | Convert a UTxO pair to a ByteString element for the accumulator.
  utxoToElementImpl :: UTxOPairType tx -> ByteString

-- | Instance for Cardano Tx - uses Plutus serialization
instance HasAccumulatorElement Tx where
  type UTxOPairType Tx = (TxIn, TxOut CtxUTxO)

  toPairList = UTxO.toList

  utxoToElementImpl (txIn, txOut) =
    toStrict (serialise $ toData $ toPlutusTxOutRef txIn)
      <> toStrict (serialise $ toData $ toPlutusTxOut txOut)
