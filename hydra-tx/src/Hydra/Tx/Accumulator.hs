{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  HydraAccumulator,
  unHydraAccumulator,
  getAccumulatorHash,
  getAccumulatorCommitment,
  computeG1CommitmentBytes,
  accumulatorSize,
  maxAccumulatorSize,
  AccumulatorTooLarge (..),
  deployedFanoutBatchSize,
  build,
  buildFromUTxO,
  buildFromSnapshotUTxOs,
  applyUTxODelta,
  removeOutputs,

  -- * CRS (Common Reference String)
  crsG2Points,
  crsG1Points,
  requiredCRSPointCount,
  defaultItems,

  -- * Membership proofs for partial fanout
  createMembershipProof,
  createMembershipProofFromUTxO,
  createCRSG2Datum,
) where

import Accumulator (Element)
import Bindings (getPolyCommitOverG1)
import Cardano.Api (BabbageEraOnwards (..), TxOutDatum (TxOutDatumInline))
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, blsCompress)
import Hydra.Cardano.Api qualified as HApi
import Hydra.Prelude
import Hydra.Tx.Accumulator.Commitment (crsG1Points, crsG2Points)
import Hydra.Tx.Accumulator.Core
import Hydra.Tx.IsTx (IsTx (..))
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element, bls12_381_G1_uncompress, bls12_381_G2_uncompress, toBuiltin)

getAccumulatorCommitment :: HydraAccumulator -> BuiltinBLS12_381_G1_Element
getAccumulatorCommitment = bls12_381_G1_uncompress . toBuiltin . _cachedCommitment

-- * Cryptographic Proofs for partial fanout

-- | Create a membership proof for a subset of UTxO elements.
--
-- This function uses getPolyCommitOverG1 from haskell-accumulator's Bindings module:
-- https://github.com/cardano-scaling/haskell-accumulator/blob/main/haskell-accumulator/lib/Bindings.hs
--
-- Given a subset of elements and the full accumulator, it:
-- 1. Removes the subset elements from the accumulator
-- 2. Computes a polynomial commitment over G1 for the remaining elements
-- 3. Returns the proof as a compressed G1 point
createMembershipProof ::
  -- | The subset of elements to prove membership of (e.g., UTxOs being fanned out)
  [Element] ->
  -- | The full accumulator from the confirmed snapshot
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point1] ->
  -- | Returns the compressed proof point, or an error if elements are missing or CRS is too short
  Either Text ByteString
createMembershipProof subsetElements fullAcc crs =
  bimap toText blsCompress $ getPolyCommitOverG1 subsetElements (unHydraAccumulator fullAcc) crs

-- | Create a membership proof from a UTxO subset.
--
-- This function extracts individual TxOut elements from the subset UTxO and proves
-- they exist in the full accumulator. The full accumulator must be built using
-- `buildFromUTxO` for this to work correctly.
--
-- The proof is verified on-chain via e(commitment_G1, G2) = e(proof_G1, P_S(τ)·G2).
createMembershipProofFromUTxO ::
  forall tx.
  IsTx tx =>
  -- | The subset of UTxO to prove membership of (e.g., UTxOs being fanned out)
  UTxOType tx ->
  -- | The full accumulator from the confirmed snapshot (built with buildFromUTxO)
  HydraAccumulator ->
  -- | Common Reference String (CRS) for the cryptographic proof
  [Point1] ->
  -- | Returns the compressed proof point, or an error if elements are missing or CRS is too short
  Either Text ByteString
createMembershipProofFromUTxO subsetUTxO fullAcc crs =
  -- Extract individual TxOut elements from the subset (each TxOut -> hash).
  -- This matches how buildFromUTxO / buildFromSnapshotUTxOs serialize each TxOut.
  -- The underlying accumulator tracks element multiplicity (via Count), so duplicate
  -- elements are handled correctly.
  -- Drop mempty: TxOuts for which toPlutusTxOut returns Nothing yield mempty here;
  -- we only prove membership of outputs that convert successfully.
  let subsetElements = filter (/= mempty) $ utxoToElement @tx <$> outputsOfUTxO @tx subsetUTxO
   in -- Use the element-based proof function
      createMembershipProof subsetElements fullAcc crs

createCRSG2Datum :: Int -> HApi.TxOutDatum ctx
createCRSG2Datum n =
  TxOutDatumInline BabbageEraOnwardsConway $
    HApi.toScriptData
      (bls12_381_G2_uncompress . toBuiltin . blsCompress <$> crsG2Points n)
