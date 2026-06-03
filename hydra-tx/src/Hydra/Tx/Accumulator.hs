{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator (
  HydraAccumulator (..),
  getAccumulatorHash,
  getAccumulatorCommitment,
  accumulatorSize,
  maxAccumulatorSize,
  build,
  buildFromUTxO,
  buildFromSnapshotUTxOs,

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

import Hydra.Prelude

import Accumulator (Accumulator, Element)
import Accumulator qualified
import Bindings (getPolyCommitOverG1)
import Cardano.Api (BabbageEraOnwards (..), TxOutDatum (TxOutDatumInline))
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress)
import Cardano.Crypto.Hash (Blake2b_256)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Data.Map.Strict qualified as Map
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Cardano.Api qualified as HApi
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.KZGTrustedSetup qualified as KZG
import Plutus.Crypto.BlsUtils (getFinalPoly, getG1Commitment, mkScalar)
import PlutusTx.Builtins (
  BuiltinBLS12_381_G1_Element,
  bls12_381_G1_compress,
  bls12_381_G2_uncompress,
  byteStringToInteger,
  fromBuiltin,
  toBuiltin,
 )

-- * HydraAccumulator

newtype HydraAccumulator = HydraAccumulator {unHydraAccumulator :: Accumulator}
  deriving newtype (Eq, Show)

build :: [ByteString] -> HydraAccumulator
build = HydraAccumulator . Accumulator.buildAccumulator

-- | Build an accumulator from a UTxO by serializing each individual TxOut.
--
-- This is the CORRECT way to build an accumulator for partial fanout proofs.
-- Each TxOut becomes a separate element in the accumulator, allowing you to later
-- prove that a subset of TxOuts was part of the original set.
--
-- The serialization matches how `hashTxOuts` works on-chain:
-- Each element = Builtins.serialiseData (toBuiltinData plutusTxOut)
--
-- Example usage:
-- > -- Build accumulator from the full UTxO set
-- > let fullAcc = buildFromUTxO @Tx utxo
-- >
-- > -- Later, prove a subset exists
-- > let crs = crsG1Points (requiredCRSPointCount fullAcc)
-- > result <- createMembershipProofFromUTxO @Tx subsetUTxO fullAcc crs
--
-- This approach allows proving that 2 out of 5 UTxOs are part of the original set,
-- which is essential for partial fanout functionality.
buildFromUTxO ::
  forall tx.
  IsTx tx =>
  -- | The UTxO set to build the accumulator from
  UTxOType tx ->
  -- | The resulting accumulator containing one element per TxOut
  HydraAccumulator
buildFromUTxO utxo =
  let elements = utxoToElement @tx <$> outputsOfUTxO @tx utxo
   in build elements

-- | Build an accumulator from snapshot UTxOs, including commit and decommit UTxOs.
--
-- Combines all UTxOs that could potentially be fanned out — main snapshot,
-- commit, and decommit — and delegates to 'buildFromUTxO' on the merged set.
-- Merging via UTxO union keeps the same canonical TxIn-sorted element order
-- used by every other accumulator call site ('computeFullFanoutUTxO',
-- 'partialFanout' staleness check, 'emitNextFanoutStep'), so the commitment
-- stored in the snapshot and all downstream proofs are built from the same
-- element set by construction.
--
-- Note: the underlying 'HydraAccumulator' is a 'Map' keyed by element bytes,
-- so insertion order is irrelevant for the commitment value; the merge is done
-- here for explicit consistency with the rest of the fanout code paths.
buildFromSnapshotUTxOs ::
  forall tx.
  IsTx tx =>
  -- | The main snapshot UTxO set
  UTxOType tx ->
  -- | UTxOs to be committed (if any)
  Maybe (UTxOType tx) ->
  -- | UTxOs to be decommitted (if any)
  Maybe (UTxOType tx) ->
  -- | The resulting accumulator containing all UTxOs
  HydraAccumulator
buildFromSnapshotUTxOs utxo mUtxoToCommit mUtxoToDecommit =
  buildFromUTxO @tx $
    utxo
      <> fromMaybe mempty mUtxoToCommit
      <> fromMaybe mempty mUtxoToDecommit

-- | Get a blake2b-256 hash of the accumulator commitment (compressed G1 point).
--
-- This is a pure function that returns a 32-byte deterministic hash of the
-- compressed G1 accumulator commitment. It is what gets signed by all parties
-- in the multi-signature and stored as 'accumulatorHash' in on-chain datums.
--
-- Hashing the compressed G1 point (rather than the serialized map) binds the
-- signed hash to the exact G1 point stored in 'ClosedDatum.accumulatorCommitment',
-- allowing the on-chain validator to verify their consistency.
getAccumulatorHash :: HydraAccumulator -> ByteString
getAccumulatorHash acc =
  digest (Proxy @Blake2b_256) . fromBuiltin . bls12_381_G1_compress $ getAccumulatorCommitment acc

-- | Number of UTxOs tracked by the accumulator.
accumulatorSize :: HydraAccumulator -> Int
accumulatorSize (HydraAccumulator acc) = sum (map snd $ Map.elems acc)

-- | Maximum accumulator size, re-exported from 'KZGTrustedSetup' for convenience.
maxAccumulatorSize :: Int
maxAccumulatorSize = KZG.maxAccumulatorSize

-- | Convert a 'KZG.KZGSetupError' 'Either' to the contained value, aborting
-- with a descriptive message if the setup is invalid.
--
-- This should never be reached in a correctly built binary: the trusted setup
-- bytes are embedded at compile time, integrity-checked via SHA-256, and
-- exercised by the test suite. A failure here would indicate binary tampering
-- or a corrupted build artefact.
fromKZGSetup :: Either KZG.KZGSetupError a -> a
fromKZGSetup = either (\e -> error $ "KZG trusted setup invariant violated: " <> show e) id

getAccumulatorCommitment :: HydraAccumulator -> BuiltinBLS12_381_G1_Element
getAccumulatorCommitment (HydraAccumulator acc) =
  let expandedElems = concatMap (\(hash, count) -> replicate count hash) $ Map.elems acc
      n = length expandedElems
   in if n > KZG.maxAccumulatorSize
        then error $ "getAccumulatorCommitment: accumulator has " <> show n <> " elements, exceeding the G1 CRS limit of " <> show KZG.maxAccumulatorSize
        else
          let crsG1 = take (n + 1) $ fromKZGSetup KZG.g1BuiltinPoints
           in getG1Commitment crsG1 . getFinalPoly . map (mkScalar . byteStringToInteger BigEndian . toBuiltin) $
                expandedElems

-- * CRS (Common Reference String)

-- | Returns the first @n@ G1 powers of tau from the EIP-4844 trusted setup.
-- Used as the off-chain CRS for building accumulator commitments and membership proofs:
-- @[G1, τ·G1, ..., τ^(n-1)·G1]@.
crsG1Points :: Int -> [Point1]
crsG1Points n = take n $ fromKZGSetup KZG.g1Points

-- | Returns the first @n@ G2 powers of tau from the EIP-4844 trusted setup.
-- Used as the on-chain CRS for verifying membership proofs:
-- @[G2, τ·G2, ..., τ^(n-1)·G2]@.
crsG2Points :: Int -> [Point2]
crsG2Points n = take n $ fromKZGSetup KZG.g2Points

defaultItems :: Int
defaultItems = 30

-- | Returns the number of G1 CRS points required for this accumulator.
-- An n-element accumulator polynomial has degree n, so needs n+1 G1 points
-- @[G1, τ·G1, ..., τⁿ·G1]@ to compute the commitment A(τ)·G1 and proofs.
-- n is the total element count including duplicates (sum of all counts).
requiredCRSPointCount :: HydraAccumulator -> Int
requiredCRSPointCount (HydraAccumulator acc) = sum (map snd $ Map.elems acc) + 1

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
createMembershipProof subsetElements (HydraAccumulator fullAcc) crs =
  bimap toText blsCompress $ getPolyCommitOverG1 subsetElements fullAcc crs

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
